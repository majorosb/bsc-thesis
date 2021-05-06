{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
module Browser
        where   

import qualified Graphics.Vty as Vty
import qualified Data.Vector as V
import System.Directory
import Object 
import Window
import Tabs
import Control.Lens
import Control.Monad.IO.Class
import Control.Exception
import System.IO
import System.IO.Error
import System.FilePath.Posix
import System.Environment
import Conduit
import Data.Vector as V (toList)
import Data.Maybe
import Data.Char
import Data.List
import Data.List.Zipper as Z
import qualified Data.Map as M


import Brick.Types
import Brick.Widgets.List as L
import Brick.Widgets.Edit
import Brick.Widgets.Border
import Brick.Widgets.Core
import Brick.Focus


data Browser = Browser { _tabs        :: Zipper (Tab)
                       , _browserName :: Name 
                       , _statusLine  :: String 
                       , _input       :: String
                       , _defEditor   :: String
                       , _bEditor     :: Editor String Name
                       , _action      :: Maybe Action 
                       , _inputMode   :: Bool
                       , _clipboard   :: Board
                       , _buffer      :: Board
                       , _winCount    :: Int
                       , _marks       :: M.Map Char FilePath
                       }

data Action  = Paste | ConfirmPaste | Rename | Move | Yank | Touch | SetMark | Mark 
        deriving (Show, Eq)

data Board = Clipboard [Object] | Cutboard [Object] | Empty

makeLenses ''Browser



statusLineW :: String -> Widget a
statusLineW s = withAttr attrStatus $ vLimit 1 $ str s

renderBrowser ::  Browser -> Widget Name
renderBrowser browser = 
        joinBorders $ vBox 
             [ tabnames <+> (withAttr attrFill $ vLimit 1 $ fill ' ')
             , joinBorders $ renderTab (focusedTab browser)
             , statusline
             ] where
                   statusline = if browser^.inputMode 
                       then statusLineW (( browser^.statusLine))<+> 
                               renderEditor (str . unlines) True (browser^.bEditor)
                       else statusLineW sLine
                   sLine = getStatusLine browser                                
                   tabnames  = foldl (<+>) emptyWidget
                         ((map (renderTabNames focusedTabName ) . collectTabNames) browser) 
                   focusedTabName = getTabName $ focusedTab browser
--                        showCursor 
--                        (browser^.browserName)
--                        (Location (length $ browser^.input,0))
--                        (str $ browser^.input)

-- Relationship between browser events are the following:
-- If the Browser has an active Action (Just someAction) then its trying to complete that action
-- until it returns Nothing. handleBrowserEvent will pass the current state and the event parameters to
-- handleBrowserAction. 
--

renderTabNames ::  String -> String -> Widget Name
renderTabNames x x' = if x == x' 
                        then withAttr tFocused . vLimit 1 . str $ x' ++ " "
                        else withAttr tEmpty . vLimit 1 . str $ x' ++ " "


newBrowser :: Name -> Tab -> IO Browser
newBrowser name tab =do
        ext <- extEditor
        return $ Browser (fromList $ tab:[]) name "" "" ext ed Nothing False Browser.Empty Browser.Empty 1 M.empty
        where ed = editor (EditName "editor")  (Just 1) ""
              extEditor = do 
                      e <- lookupEnv "EDITOR" 
                      case e of 
                        Just a -> return a
                        Nothing -> return "nano"
--- EventHandlers ---
handleBrowserEvent :: Vty.Event -> Browser -> EventM Name Browser
handleBrowserEvent ev browser = case browser^.action of              -- if there is an active action
     Just someAction -> handleBrowserAction ev someAction browser    -- handle the action 
     Nothing         -> handleBrowserNormal ev browser               -- else regular window events are called

handleBrowserNormal :: Vty.Event -> Browser -> EventM Name Browser
handleBrowserNormal ev = case ev of 
     Vty.EvKey (Vty.KChar 'r')  []  -> browserSetAction Rename "Rename: " True -- activating the Rename action in the state
     Vty.EvKey (Vty.KChar 'i')  []  -> browserSetAction Touch "Touch: " True -- activating the Touch action in the state
     Vty.EvKey (Vty.KChar 't')  []  -> addNewTab 
     Vty.EvKey (Vty.KChar 'y')  []  -> browserSetAction Yank "Yank" False
     Vty.EvKey (Vty.KChar 'x')  []  -> cutSelectedObjects 
     Vty.EvKey (Vty.KChar 'p')  []  -> handleCopy 
     Vty.EvKey (Vty.KChar 'b')  []  -> moveTabBackward 
     Vty.EvKey (Vty.KChar 'w')  []  -> moveTabForward 
     Vty.EvKey (Vty.KChar 'o')  []  -> handleOrdObjects 
     Vty.EvKey (Vty.KChar 'H')  []  -> handleHSplit 
     Vty.EvKey (Vty.KChar 'V')  []  -> handleVSplit
     Vty.EvKey (Vty.KChar 'd')  []  -> deleteWindow
     Vty.EvKey (Vty.KChar 'm')  []  -> browserSetAction SetMark "Setting mark" False
     Vty.EvKey (Vty.KLeft)      []  -> handleShiftFocus Tabs.Left
     Vty.EvKey (Vty.KRight)     []  -> handleShiftFocus Tabs.Right
     Vty.EvKey (Vty.KUp)        []  -> handleShiftFocus Tabs.Up
     Vty.EvKey (Vty.KDown)      []  -> handleShiftFocus Tabs.Down
     otherwise                      -> handleOtherEvents ev

handleOtherEvents :: Vty.Event -> Browser -> EventM Name Browser
handleOtherEvents ev b = do
             w' <- handleWindowEvent ev w                    -- Handle non action related event-keys
             case (w'^.windowException) of
                 Just e  -> return $ handleIOException e b
                 Nothing -> return $ b & tabs.~(Z.insert (refreshTab t w') $ Z.delete ts)
     where
            w   = focusedWindow b
            ts  = b^.tabs
            t   = focusedTab b

--Actions--
handleBrowserAction ::  Vty.Event -> Action -> Browser -> EventM Name Browser
handleBrowserAction e a =  case a of
     Rename       -> handleWithInput handleRenameAction e 
     Paste        -> handleCopyAction 
     ConfirmPaste -> handleWithInput handleCopyConfirm e 
     Touch        -> handleWithInput handleTouchAction e 
     Yank         -> handleYank e
     SetMark      -> handleSetMark e
     _            -> return  

handleSetMark :: Vty.Event -> Browser -> EventM Name Browser
handleSetMark (Vty.EvKey( Vty.KChar x) []) b = return $ b' & marks.~m
        where wPath = (focusedWindow b)^.currentDir
              m = M.insert x wPath $ b^.marks
              b' = browserFinishAction ("Mark set to " ++ (x : " -> " ++ wPath)) b
handleSetMark (Vty.EvKey _ []) b = return $ browserFinishAction "Invalid key to set a mark" b

handleYank :: Vty.Event -> Browser -> EventM Name Browser
handleYank e = case e of 
      Vty.EvKey (Vty.KChar 'y') [] -> copySelectedObjects
      Vty.EvKey (Vty.KChar 'a') [] -> copyCurrDir
      Vty.EvKey (Vty.KChar 'i') [] -> undefined --  copyCurrDirFiles
      Vty.EvKey (Vty.KChar 't') [] -> copyTab
      _                            -> return . browserFinishAction "" 

copyTab :: Browser -> EventM Name Browser
copyTab b = do 
        let b' = browserFinishAction ("Copied " ++ (show . length $ l) ++ " files from this tab") b
        return $ b' & clipboard.~clip & buffer.~clip
        where tree = (focusedTab b)^.renderT
              l    = nub . concat . map (\n -> selectedObjects n) . foldr (:) [] $ tree
              clip = Clipboard $ l
safePaste :: Browser -> EventM Name Browser
safePaste b@Browser {_buffer=Cutboard (x:xs)} = case (x^.filetype) of 
       Directory -> do
         t <- liftIO $ tryPasteDir
         handlePasteDir t
       _         ->  do 
         t <- liftIO $ tryPasteFile 
         handlePasteFile t
  where
          w      = focusedWindow b
          tryPasteDir :: IO (Either SomeException ())
          tryPasteDir   = try $ pasteDirectoryRecursive (x^.path) $ w^.currentDir </> x^.name 
          tryPasteFile :: IO (Either SomeException ())
          tryPasteFile  = try $ copyFile (x^.path) $ w^.currentDir </> x^.name
          handlePasteDir t  = either 
                             (\e -> return $ browserFinishAction ("Error while copying dir: " ) b)
                             (\_ -> do liftIO $ removeDirectoryRecursive (x^.path)
                                       b' <- browserSetAction Paste ("Copied " ++ x^.name) False b
                                       handleCopyAction $ b' & buffer.~(Cutboard xs)) t
          handlePasteFile t = either 
                                   (\e -> return $ browserFinishAction ("Error while copying file: " ) b)
                                   (\_ -> do liftIO $ removeFile (x^.path)
                                             b' <- browserSetAction Paste ("Copied " ++ x^.name) False b
                                             handleCopyAction $ b' & buffer.~(Cutboard xs)) t

safeCopyPaste :: Browser -> EventM Name Browser 
safeCopyPaste  b@Browser {_buffer=Clipboard (x:xs)} = do 
        case (x^.filetype) of 
            Directory -> liftIO $ pasteDirectoryRecursive (x^.path) $ w^.currentDir </> x^.name 
            _         -> liftIO $ copyFile (x^.path) $ w^.currentDir </> x^.name
        b' <- browserSetAction Paste ("Copied " ++ x^.name) False b
        handleCopyAction $ b' & buffer.~(Clipboard xs)
        where
                w = focusedWindow b


pasteOneObject :: Bool -> Browser -> EventM Name Browser --the bool indicates if it has been confirmed
pasteOneObject c b@Browser {_buffer=Clipboard (x:xs)} = do
        contents <- liftIO $ getDirectoryContents $ w^.currentDir
        case c of 
          True -> safeCopyPaste b
          False -> if any (\n -> n == x^.name) contents
           then browserSetAction ConfirmPaste msg True b 
           else safeCopyPaste b
        where 
              w      = focusedWindow b
              msg    = x^.name ++ " already exists, do you want to replace it?(y/n)" 

pasteOneObject c b@Browser {_buffer=Cutboard (x:xs)} = do
        contents <- liftIO $ getDirectoryContents $ w^.currentDir
        case c of 
          True -> safePaste b
          False -> if any (\n -> n == x^.name) contents
                   then browserSetAction ConfirmPaste msg True b 
                   else safePaste b
         where
             w      = focusedWindow b
             msg    = x^.name ++ " already exists, do you want to replace it?(y/n)" 

handleCopyAction :: Browser -> EventM Name Browser
handleCopyAction b@Browser {_clipboard=Browser.Empty} = return b
handleCopyAction b@Browser {_buffer=Clipboard []} = do
        t' <- liftIO $ refreshFocusedW t w
        b' <- refreshFocusedTab . browserReset $ b
        return $ b' & statusLine.~"Copy complete" & buffer.~clip
                where t  = focusedTab b
                      w  = focusedWindow b
                      clip = b^.clipboard
handleCopyAction b@Browser {_buffer=Cutboard []} = do  --the same but we set the clipboard to empty 
        t' <- liftIO $ refreshFocusedW t w
        b' <- refreshFocusedTab . browserReset $ b
        return $ b' & statusLine.~"Copy complete" 
                    & buffer.~Browser.Empty
                    & clipboard.~Browser.Empty
                where t  = focusedTab b
                      w  = focusedWindow b
                      clip = b^.clipboard
handleCopyAction b = do
        contents <- liftIO $ getDirectoryContents $ w^.currentDir
        if any (\n -> n == x^.name) contents
           then browserSetAction ConfirmPaste msg True b 
           else pasteOneObject False b
        where 
              (x:xs) = fromBoard $  b^.buffer
              w      = focusedWindow b
              msg    = x^.name ++ " already exists, do you want to replace it?(y/n)" 

handleRenameAction :: Vty.Event -> Browser -> EventM Name Browser
handleRenameAction k = handleBrowserRename 

handleTouchAction :: Vty.Event -> Browser -> EventM Name Browser
handleTouchAction k  = handleTouch 

browserSetAction :: Action ->  String -> Bool -> Browser -> EventM Name Browser 
browserSetAction a msg f browser = return $ browser & action?~a 
                            & inputMode.~f & statusLine.~msg & input.~"" 
handleWithInput :: (Vty.Event -> Browser -> EventM Name Browser)-> Vty.Event -> Browser  -> EventM Name Browser
handleWithInput f e b = if b^.inputMode then handleBrowserInput e b else f e b

--Copy--
handleCopy :: Browser -> EventM Name Browser
handleCopy b = do 
        b' <- browserSetAction Paste "Copying files" False b
        handleCopyAction b'
        where num = (length .fromBoard $ b^.clipboard)

handleCopyConfirm :: Vty.Event -> Browser -> EventM Name Browser
handleCopyConfirm _ b@Browser {_buffer=Clipboard []}  = return $ browserReset b & buffer.~Browser.Empty
handleCopyConfirm _ b@Browser {_buffer=Cutboard  []}  = return $ browserReset b & clipboard.~Browser.Empty & buffer.~Browser.Empty
handleCopyConfirm e b@Browser {_buffer=Clipboard  x}
  | map toLower (b^.input) == "y" = pasteOneObject True b
  | map toLower (b^.input) == "n" = handleSkipCopy x b 
  | otherwise                     = handleSkipCopy x b
 where
        inp = b^.input

handleCopyConfirm e b@Browser {_buffer=Cutboard x}
  | map toLower (b^.input) == "y" = pasteOneObject True b
  | map toLower (b^.input) == "n" = handleSkipCopy x b 
  | otherwise                     = handleSkipCopy x b
 where
        inp = b^.input
        
handleReplaceCopy :: [Object] ->  Browser -> EventM Name Browser
handleReplaceCopy (x:xs) b = do 
        case (x^.filetype) of 
            Directory -> liftIO $ pasteDirectoryRecursive (x^.path) $ w^.currentDir </> x^.name 
            _ -> liftIO $ copyFile (x^.path) $ w^.currentDir </> x^.name
        b'' <- b'
        handleCopyAction $ b'' & buffer.~(Clipboard xs)
                where
                   b' = browserSetAction Paste "" False b
                   w  = focusedWindow b


handleSkipCopy :: [Object] -> Browser -> EventM Name Browser
handleSkipCopy (x:xs) b = do
        b'' <- b'
        handleCopyAction $ b'' & buffer.~(Clipboard xs)
        where b' = browserSetAction Paste "" False b

getSelectedElement :: Browser -> EventM Name Browser
getSelectedElement b = case listSelectedElement (w^.objects) of 
        Just (_,obj) -> return $ b & buffer.~(Clipboard [obj]) 
                                   & clipboard.~(Clipboard [obj])
                                   & action.~Nothing
        Nothing      -> return b
        where w = focusedWindow b


copySelectedObjects :: Browser -> EventM Name Browser
copySelectedObjects b = if s == [] then getSelectedElement b
                                   else return $ b 
                                               & buffer.~clip 
                                               & clipboard.~clip
                                               & action.~Nothing
                                               & statusLine.~msg
        where 
              s =  selectedObjects (t^.focused)
              clip = Clipboard s
              t = focusedTab b
              msg = "Copied " ++ (show . length $ s) ++ " files to clipboard"

cutSelectedObjects :: Browser -> EventM Name Browser
cutSelectedObjects b = return $ b & buffer.~clip & clipboard.~clip
        where clip = Cutboard $ selectedObjects (t^.focused)
              t = focusedTab b

copyCurrDir :: Browser -> EventM Name Browser
copyCurrDir b = return $ b & buffer.~clip 
                           & clipboard.~clip
                           & action.~Nothing 
                           & statusLine.~msg
        where clip = Clipboard $ (V.toList . listElements $ w^.objects)
              w = focusedWindow b
              msg = "Current directory copied to clipboard with " ++ (show . length . fromBoard $ clip) ++ " objects"

pasteDirectoryRecursive ::  FilePath -> FilePath -> IO ()
pasteDirectoryRecursive src dst = runConduitRes
  $ sourceDirectoryDeep False src
  .| mapC (\fPath -> (fPath, dst </> (makeRelative src fPath)))
  .| iterMC (liftIO . createDirectoryIfMissing True . takeDirectory . snd)
  .| mapM_C (\(srcFile, dstFile)  -> liftIO $ copyFile srcFile dstFile) 

--Input--
handleBrowserInput :: Vty.Event ->  Browser -> EventM Name Browser  
handleBrowserInput ev browser = 
 case ev of
     Vty.EvKey Vty.KEsc         []  -> return $ browserReset browser
     Vty.EvKey Vty.KEnter       []  -> handleBrowserEvent ev $ browser & input.~inputS & inputMode.~False
     _                              -> do
            newEditor <- handleEditorEvent ev (browser^.bEditor)
            return $ browser & bEditor.~newEditor
 where
     inputS :: String
     inputS = concat $ getEditContents $ browser^.bEditor

handleOrdObjects :: Browser -> EventM Name Browser 
handleOrdObjects browser = return $ browser & tabs.~(refreshTabZipper ts newTab)
        where w      = focusedWindow browser
              w'     = w & objects.~ordObjects w
              t      = focusedTab browser
              newTab = t & focused.~w'
              ts     = browser^.tabs


handleTouch :: Browser -> EventM Name Browser
handleTouch b = if isValid (b^.input) 
                  then do 
                          tmp <- liftIO $ openTempFile dir (b^.input)
                          liftIO $ renameFile (fst tmp) (b^.input)
                          liftIO $ refreshFocusedW t w 
                          return $ b' & tabs.~(refreshTabZipper tz t)
                  else returnBrowserError "Invalid filename" b
        where
                w    = focusedWindow b
                t    = focusedTab b
                tz   = b^.tabs
                dir  = w^.currentDir
                b'   = browserFinishAction (b^.input ++ " created with read and write permissions") b
                pred = isValid (b^.input)

handleBrowserRename :: Browser -> EventM Name Browser 
handleBrowserRename browser = 
     case oldM of
         Just old  -> case old^.filetype of 
                        Directory ->if isValid new
                                     then do                             
                                       liftIO $ renameDirectory (old^.name) new
                                       return $ browserFinishAction ("Success new filename is: " ++ new) browser
                                     else returnBrowserError "Invalid filename" browser 
                        _         -> if isValid new
                                     then do                             
                                       liftIO $ renameFile (old^.name) new
                                       return $ browserFinishAction ("Success new filename is: " ++ new) browser
                                     else returnBrowserError "Invalid filename" browser 
         Nothing -> returnBrowserError "Something went wrong" browser 
     where 
        new = browser^.input
        oldM :: Maybe Object
        oldM = do
           (_,item) <- listSelectedElement ((focusedWindow browser)^.objects)
           return $ item


--Tab and window operations--                       
refreshTabZipper :: Zipper(Tab) -> Tab -> Zipper(Tab)
refreshTabZipper z t = (Z.insert t $ Z.delete z)

deleteWindow :: Browser -> EventM Name Browser
deleteWindow b = return $ b & tabs.~(refreshTabZipper (b^.tabs) t')
        where t = focusedTab b 
              w = focusedWindow b
              tree = t^.renderT
              tree' = deleteTree w tree
              w'    = getWindow tree'
              ring' = focusSetCurrent (w'^.windowName) (t'^.ring)
              t'    = t & renderT.~tree' & focused.~w' & ring.~ring'


handleHSplit :: Browser -> EventM Name Browser
handleHSplit b = do 
        w' <- liftIO $ newWindow (WindowName ((b^.winCount) + 1)) "."
        let newT = hSplitWindow (focusedTab b) w' Tabs.Horizontal
        return $ b & tabs.~(refreshTabZipper (b^.tabs) newT) & winCount.~((b^.winCount) + 1)
               
handleVSplit :: Browser -> EventM Name Browser
handleVSplit b = do 
        w' <- liftIO $ newWindow (WindowName ((b^.winCount) + 1)) "."
        let newT = hSplitWindow (focusedTab b) w' Tabs.Vertical
        return $ b & tabs.~(refreshTabZipper (b^.tabs) newT) & winCount.~((b^.winCount) + 1)

makeNewTabName :: Browser -> Int -> String -> Name
makeNewTabName browser c name  = if elem newName names 
                                    then makeNewTabName browser (c+1) name
                                    else newName
        where ts      = Z.toList (browser^.tabs)
              names   = getTabNames ts

              newName = case c of 
                         0 -> TabName name
                         _ -> TabName (name ++ " (" ++ show c ++ ")")

moveTabForward :: Browser -> EventM Name Browser 
moveTabForward browser = return $ browser & tabs.~z
                where
                        w = browser^.tabs
                        z = if endp (right w) then start w else right w

moveTabBackward :: Browser -> EventM Name Browser 
moveTabBackward browser = return $ browser & tabs.~z
                where
                        w = browser^.tabs 
                        z = if beginp w then left (end w) else left w 
          --V.EvKey V.KEnd []   -> M.halt b

handleShiftFocus :: Movement -> Browser -> EventM Name Browser
handleShiftFocus m b = return $ b & tabs.~t'
        where t' = refreshTabZipper (b^.tabs) (shiftFocus m $ focusedTab b)

refreshFocusedTab :: Browser -> EventM Name Browser
refreshFocusedTab b@(Browser {_tabs = tz}) = do 
      t' <- liftIO $ refreshFocusedW t (t^.focused)
      let tz' = refreshTabZipper tz t' 
      return $ b & tabs.~tz'
      where
         t = cursor tz
         
getTabNames :: [Tab] -> [Name]
getTabNames [(Tab {_tabName=n}) ]    = [n]
getTabNames ((Tab {_tabName=n}):xs)  = n : getTabNames xs

addNewTab :: Browser -> EventM Name Browser 
addNewTab browser = do
        win <- liftIO $ newWindow (WindowName $ browser^.winCount + 1) "."
        dirName <- liftIO $ getCurrentDirectory
        let n = makeNewTabName browser 0 dirName
        let t = newTab n win
        return $ browser & tabs.~(Z.insert t (browser^.tabs)) & winCount.~count
                where
                        w     = focusedWindow browser
                        count =  browser^.winCount + 1

focusedWindow :: Browser -> Window
focusedWindow b = t^.focused
        where t = focusedTab b

focusedTab :: Browser -> Tab
focusedTab b = cursor $ b^.tabs

getTabName :: Tab -> String
getTabName t =(\(TabName s) -> s) $ t^.tabName

fTabName :: [String] -> Zipper Tab -> [String]
fTabName x t = s : x
        where s = getTabName $ cursor t

collectTabNames :: Browser ->  [String]
collectTabNames b = foldlz fTabName [] t
        where t = start $ b^.tabs
        

                               
--- Utility ---

browserFinishAction :: String   ->    -- the message to be written to the statusline
                       Browser  ->    -- current state
                       Browser 
browserFinishAction message browser = browser & 
     inputMode.~False     &               -- disabling inputMode
     statusLine.~message  & 
     action.~Nothing      &
     bEditor.~emptyEditor
                         
emptyEditor :: Editor String Name
emptyEditor = editor (EditName "e") (Just 1) ""

browserReset :: Browser -> Browser 
browserReset browser = browser &
     action.~Nothing  & 
     inputMode.~False &
     statusLine.~""   &
     input.~""        &
     bEditor.~editor (EditName "e") (Just 1) ""        -- resetting the editor

handleIOException :: IOError -> Browser -> Browser 
handleIOException e browser = 
     case isPermissionError e of
        True  -> browser & statusLine .~ "Permission error"
        _     -> browser & statusLine .~ "error"
        --  False -> browser

getStatusLine :: Browser -> String
getStatusLine b = b^.statusLine


fromBoard :: Board -> [Object]
fromBoard Browser.Empty = []
fromBoard (Clipboard l) = l
fromBoard (Cutboard  l) = l
--validateFilename ::  String -> Bool
--validateFilename b name = isValid name 
--                           then handleBrowserRename b

returnBrowserError :: String -> Browser -> EventM Name Browser 
returnBrowserError e browser = return $
        browser & inputMode.~False & statusLine.~e & action.~Nothing & input.~""

selectedObjects :: Window -> [Object]
selectedObjects window = filter (\n -> n^.isSelected) l
        where
                l = V.toList . listElements $ window^.objects 

ordObjects :: Window -> List Name Object
ordObjects window = list resourceName (V.fromList $ sort l) 1
        where
                l = V.toList . listElements $ window^.objects 
                resourceName  = window^.windowName

