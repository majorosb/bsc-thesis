{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Browser
        where   
import qualified Graphics.Vty as Vty
import qualified Control.Exception as E
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as V
import qualified System.Directory as Dir

import Object 
import Window
import Tabs
import Control.Lens
import Control.Monad.IO.Class
import System.IO.Error
import System.FilePath.Posix
import Data.Maybe
import Data.Char
import Data.List.Zipper as Z
import Data.List

import Brick.Types
import Brick.Widgets.List as L
import Brick.Widgets.Edit
import Brick.Widgets.Border
import Brick.Widgets.Core


data Browser = Browser { _tabs        :: Zipper (Tab)
                       , _browserName :: Name 
                       , _statusLine  :: String 
                       , _input       :: String 
                       , _bEditor     :: Editor String Name
                       , _action      :: Maybe Action 
                       , _inputMode   :: Bool
                       , _clipboard   :: Board
                       , _winCount    :: Int
                       }

data Board = Clipboard [Object] | Cutboard [Object] | Empty
data Action  = Paste | ConfirmPaste |  Rename | Move deriving Show

makeLenses ''Browser

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
        

statusLineW :: String -> Widget a
statusLineW s = vLimit 1 $ str s

renderBrowser ::  Browser -> Widget Name
renderBrowser browser = 
        joinBorders $ vBox 
             [tabnames
             , joinBorders $ renderTab (focusedTab browser)
             , statusline
             ] where
                   statusline = if browser^.inputMode 
                       then statusLineW (( show $ browser^.statusLine)++ ": ")<+> 
                               renderEditor (str . unlines) True (browser^.bEditor)
                       else statusLineW sLine
                   sLine = getStatusLine browser                                
                   tabnames  = foldl (<+>) emptyWidget
                         ((map (\n -> vLimit 1 $ str n <+> vBorder) . collectTabNames) browser)
--                        showCursor 
--                        (browser^.browserName)
--                        (Location (length $ browser^.input,0))
--                        (str $ browser^.input)

-- Relationship between browser events are the following:
-- If the Browser has an active Action (Just someAction) then its trying to complete that action
-- until it returns Nothing. handleBrowserEvent will pass the current state and the event parameters to
-- handleBrowserAction. 
--

handleBrowserAction ::  Vty.Event -> Action -> Browser -> EventM Name (Browser)
handleBrowserAction e a b =  case a of
     Rename       -> handleRenameAction e b
     Paste        -> handleCopyAction clip b
     ConfirmPaste -> handleCopyConfirm e clip b
     _            -> return b 
 where
     newBrowserState e a b = handleBrowserInput e b 
     mode :: Bool
     mode = b^.inputMode
     clip = fromBoard $ b^.clipboard

newBrowser :: Name -> Tab ->  IO Browser
newBrowser name tab = return $ Browser (fromList $ tab:[]) name "" "" ed Nothing False Browser.Empty 1
        where ed = editor (EditName "editor")  (Just 1) ""

--- Events ---
handleBrowserEvent :: Vty.Event -> Browser -> EventM Name Browser
handleBrowserEvent ev browser = case browser^.action of              -- if there is an active action
     Just someAction -> handleBrowserAction ev someAction browser    -- handle the action 
     Nothing         -> handleBrowserNormal ev browser               -- else regular window events are called

handleBrowserNormal :: Vty.Event -> Browser -> EventM Name Browser
handleBrowserNormal ev browser = case ev of 
     Vty.EvKey (Vty.KChar 'r')  []  -> browserSetAction Rename "Rename: " True browser  -- activating the Rename action in the state
     Vty.EvKey (Vty.KChar 't')  []  -> addNewTab browser
     Vty.EvKey (Vty.KChar 'y')  []  -> copySelectedObjects browser 
     Vty.EvKey (Vty.KChar 'p')  []  -> handleCopy browser
     Vty.EvKey (Vty.KChar 'b')  []  -> moveTabBackward browser
     Vty.EvKey (Vty.KChar 'w')  []  -> moveTabForward browser
     Vty.EvKey (Vty.KChar 'o')  []  -> handleOrdObjects browser
     Vty.EvKey (Vty.KChar 'H')  []  -> handleHSplit browser 
     Vty.EvKey (Vty.KChar 'V')  []  -> handleVSplit browser 
     Vty.EvKey (Vty.KChar 'f')  []  -> handleShiftFocus browser
     otherwise                      -> do 
             w' <- handleWindowEvent ev w                    -- Handle non action related event-keys
             case (w'^.windowException) of
                 Just e  -> return $ handleIOException e browser 
                 Nothing -> return $ browser & tabs.~(Z.insert (refreshTab t (w' , dir) ) $ Z.delete ts)
     where
            w   = focusedWindow browser
            ts  = browser^.tabs
            t   = focusedTab browser
            dir = case findWindow (w^.windowName) t of
                    Just (_,d)  -> d
                    Nothing     -> Tabs.Horizontal

refreshTabZipper :: Zipper(Tab) -> Tab -> Zipper(Tab)
refreshTabZipper z t = (Z.insert t $ Z.delete z)

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
                                    else case c of 
                                       1 -> TabName name
                                       _ -> TabName (name ++ " (" ++ show c ++ ")")
        where ts      = Z.toList (browser^.tabs)
              names   = getTabNames ts
              newName = TabName name 
       -- rewrite it with has


fromBoard :: Board -> [Object]
fromBoard (Clipboard l) = l
fromBoard (Cutboard  l) = l

handleCopy :: Browser -> EventM Name Browser
handleCopy b = do 
        b' <- browserSetAction Paste "Copying files" False b
        handleCopyAction clip b'
     where clip = fromBoard $ b^.clipboard

handleCopyAction :: [Object] -> Browser -> EventM Name (Browser)
handleCopyAction [] b      = return $ browserReset b
handleCopyAction (x:xs) b  = do
        contents <- liftIO $ Dir.getDirectoryContents $ w^.currentDir
        if any (\n -> n == x^.name) contents
           then do 
                   b' <- browserSetAction ConfirmPaste msg True b
                   return $ b'
           else do 
                   liftIO $ Dir.copyFile (x^.path) $ w^.currentDir ++ "/" ++ x^.name
                   return $ b & clipboard.~(Clipboard xs)
        where 
              w      = focusedWindow b
              msg    = x^.name ++ " already exists, do you want to replace it?(y/n)" 

handleCopyConfirm :: Vty.Event -> [Object]  -> Browser -> EventM Name Browser
handleCopyConfirm _ [] b = return $ browserReset b
handleCopyConfirm e x b 
  | inp                    == ""  = handleBrowserInput e b 
  | map toLower (b^.input) == "y" = handleReplaceCopy x b 
  | map toLower (b^.input) == "n" = handleSkipCopy x b 
  | otherwise                 = return $ b
 where
        inp = b^.input
        
handleReplaceCopy :: [Object] ->  Browser -> EventM Name Browser
handleReplaceCopy (x:xs) b = do
        liftIO $ Dir.copyFile (x^.path) $ w^.currentDir ++ "/" ++ x^.name
        b'' <- b'
        handleCopyAction xs $ b'' & clipboard.~(Clipboard xs)
                where
                   b' = browserSetAction Paste "" False b
                   w  = focusedWindow b

handleSkipCopy :: [Object] ->  Browser -> EventM Name Browser
handleSkipCopy (x:xs) b = do
        b'' <- b'
        return $ b'' & clipboard.~(Clipboard xs)
        where b' = browserSetAction Paste "" False b

handleBrowserInput :: Vty.Event ->  Browser -> EventM Name Browser  
handleBrowserInput ev browser = 
 case ev of
     Vty.EvKey Vty.KEsc         []  -> return $ browserReset browser
     Vty.EvKey Vty.KEnter       []  -> handleBrowserEvent ev $ browser & input.~inputS
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

handleRenameAction :: Vty.Event -> Browser -> EventM Name Browser
handleRenameAction k b | b^.input == "" = handleBrowserInput k b
                       | b^.input /= "" = handleBrowserRename b


--handleCopyAction :: Vty.Event -> Browser -> EventM Name Browser
--handleCopyAction k b = do 
--        case b^.clipboard of 
--                  Cutboard  l ->  liftIO $ mapM_ 
--                        (\n -> Dir.copyFile (n^.path) $ w^.currentDir ++ "/" ++ n^.name) l
--                  Clipboard l ->  liftIO $ mapM_ 
--                        (\n -> Dir.copyFile (n^.path) $ w^.currentDir ++ "/" ++ n^.name) l
--        w' <- liftIO $ refreshWindow w
--        return $ b & tabs.~(refreshTabZipper z $ refreshTab t (w',d))
--        where
--                Just (w,d) = findWindow (t^.focused . windowName) t --this is not good
--                t          = focusedTab b 
--                z          = b^.tabs

handleBrowserRename :: Browser -> EventM Name Browser 
handleBrowserRename browser = 
     case old of
         Just _  -> if isValid new
                     then do                             
                       --liftIO $ Dir.renameFile a new
                       return $ browserFinishAction ("Success new filename is: " ++ new) browser
                     else returnBrowserError "Invalid filename" browser 
         Nothing -> returnBrowserError "Something went wrong" browser 
     where 
        new = browser^.input
        old :: Maybe String
        old = do
           (_,item) <- listSelectedElement ((focusedWindow browser)^.objects)
           return $ item^.name


handleShiftFocus :: Browser -> EventM Name Browser
handleShiftFocus b = return $ b & tabs.~t'
        where t' = refreshTabZipper (b^.tabs) (shiftFocus $ focusedTab b)


getTabNames :: [Tab] -> [Name]
getTabNames [(Tab n _ _ _ _ ) ]    = [n]
getTabNames ((Tab n _ _ _ _ ):xs)  = n : getTabNames xs

addNewTab :: Browser -> EventM Name Browser 
addNewTab browser = do
        win <- liftIO $ newWindow (WindowName $ browser^.winCount + 1) "."
        dirName <- liftIO $ Dir.getCurrentDirectory
        let n = makeNewTabName browser 1 dirName
        let t = newTab n win
        return $ browser & tabs.~(Z.insert t (browser^.tabs)) & winCount.~count
                where
                        w     = focusedWindow browser
                        count =  browser^.winCount + 1
                       

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
selectedObjects :: Window -> [Object]
selectedObjects window = filter (\n -> n^.isSelected) l
        where
                l = V.toList . listElements $ window^.objects 

ordObjects :: Window -> List Name Object
ordObjects window = list resourceName (V.fromList $ sort l) 1
        where
                l = V.toList . listElements $ window^.objects 
                resourceName  = window^.windowName

copySelectedObjects :: Browser -> EventM Name Browser
copySelectedObjects browser =return $ browser & clipboard.~clip
        where clip = Clipboard $ selectedObjects (t^.focused)
              t = focusedTab browser

                               
--- Utility ---

browserSetAction :: Action ->  String -> Bool -> Browser -> EventM Name Browser 
browserSetAction a msg f browser = return $ browser & action.~(Just a) 
                            & inputMode.~f & statusLine.~msg & input.~"" 

browserFinishAction :: String       ->    -- the message to be written to the statusline
                       Browser      ->    -- current state
                       Browser 
browserFinishAction message browser = browser & 
     inputMode.~False     &               -- disabling inputMode
     statusLine.~message  & 
     action.~Nothing      &
     bEditor.~editor (EditName "e") (Just 1) ""        -- resetting the editor
                         

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

--validateFilename ::  String -> Bool
--validateFilename b name = isValid name 
--                           then handleBrowserRename b

returnBrowserError ::  String -> Browser -> EventM Name (Browser )
returnBrowserError e browser = return $
        browser & inputMode.~False & statusLine.~e & action.~Nothing & input.~""
