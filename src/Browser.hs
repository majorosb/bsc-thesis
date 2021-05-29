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
import Control.Monad
import Control.Monad.IO.Class
import Control.Exception
import System.IO
import System.FilePath.Posix
import System.Environment
import Conduit
import Data.Char
import Data.List as DL
import Data.List.Zipper as Z
import Data.Vector as V (toList)
import qualified Data.Map as M
import qualified System.Posix.RawFilePath.Directory as PR
import Data.ByteString.Char8 (pack)
import Brick.Types
import Brick.Widgets.List as L
import Brick.Widgets.Edit
import Brick.Widgets.Core
import Brick.Widgets.Center
import qualified Brick.AttrMap as A
import Brick.Focus

-- | This is the application's state type. It controls the event cycle with action, and inputMode.
data Browser = 
   Browser { _tabs        :: Zipper(Zipper Tab) -- ^ The workspace. Inside each workspace there is a collection of tabs.
           , _browserName :: Name                 -- ^ Resource name for Brick
           , _statusLine  :: String               -- ^ This is the notification bar of the application. It gets rendered at the bottom part of the UI. 
           , _input       :: String               -- ^ The input string after user hits Enter.
           , _defEditor   :: String               -- ^ The default external editor
           , _bEditor     :: Editor String Name   -- ^ Editor provided by the Brick library.
           , _action      :: Maybe Action         -- ^ Current action
           , _inputMode   :: Bool                 -- ^ Whether the application is in input mode or not. 
           , _clipboard   :: Board 
           , _buffer      :: Board                -- ^ A copy of clipboard. It is used to keep track of the state during copy.
           , _winCount    :: Int                  -- ^ This provides unique names for the Window type.
           , _wspace      :: Int                  -- ^ Current workspace
           , _marks       :: M.Map Char FilePath 
           , _order       :: (Ordering',OrderBy)
           }

-- | The actions that the filemanager can make. 
data Action  = Paste | ConfirmPaste | Rename | RenameObj | RenameTab | Move | Yank | Touch | TouchF | SetMark | Mark | Delete | Go | OrderObj
        deriving (Show, Eq)

data OrderBy = ByName | BySize

data Ordering' = Ascending | Descending

data Board = Clipboard [Object] | Cutboard [Object] | Empty

makeLenses ''Browser

statusLineW :: String -> Widget a
statusLineW s = withAttr attrStatus $ vLimit 2 $ str s

renderTabNames ::  String -- ^ The focused tab's name.
               -> String
               -> Widget Name
renderTabNames x x' = if x == x' 
                        then withAttr tFocused . vLimit 1 . str $ x' ++ " "
                        -- use the tFocused attribute to distinguish the focused tab from the other tabs
                        else withAttr tEmpty . vLimit 1 . str $ x' ++ " "

renderBrowser ::  Browser -> Widget Name
renderBrowser b = 
        joinBorders $ vBox 
             [withAttr attrFill . hCenter $ pathW     -- the focused window's current path
             , tabnames <+> withAttr attrFill  (vLimit 1 $ fill ' ') 
                        <+> (withAttr tEmpty . str $ "[" ++ show  (b^.wspace) ++ "]")
             , joinBorders $ renderTab (focusedTab b)
             , statusline
             ] where
                   statusline = if b^.inputMode 
                       then statusLineW ( b^.statusLine)<+> 
                               renderEditor (str . unlines) True (b^.bEditor)
                       else statusLineW sLine
                   sLine = getStatusLine b
                   tabnames  = foldl (<+>) emptyWidget -- get all the tab's names
                         ((map (renderTabNames focusedTabName ) . collectTabNamesFocused) b) 
                   focusedTabName = getTabName $ focusedTab b
                   pathW =  str $ focusedWindow b ^. currentDir 



newBrowser :: Name -> Tab -> IO Browser
newBrowser rName tab =do
        ext <- extEditor
        return $ Browser{
                        _tabs = Z.insert (fromList [tab]) (Z.empty :: Zipper(Zipper Tab))
                        ,_browserName = rName 
                        ,_statusLine = "Press Q to quit or press ? for help."
                        ,_input = ""
                        ,_defEditor = ext
                        ,_bEditor = ed
                        ,_action = Nothing
                        ,_inputMode = False
                        ,_buffer = Browser.Empty
                        ,_clipboard= Browser.Empty
                        ,_winCount = 4
                        ,_wspace   = 1
                        ,_marks = M.empty
                        ,_order = (Ascending, ByName)
                        }
        where ed = editor (EditName "editor")  (Just 1) ""
              extEditor = do 
                      e <- lookupEnv "EDITOR" 
                      case e of 
                        Just a -> return a
                        Nothing -> return "nano"


--- EventHandlers ---

-- | Relationship between browser events are the following:
-- If the Browser has an active Action (Just someAction) then its trying to complete that action
-- until it returns Nothing. handleBrowserEvent will pass the current state and the event parameters to
-- handleBrowserAction or to handleBrowserAction . 
handleBrowserEvent :: Vty.Event -> Browser -> EventM Name Browser
handleBrowserEvent ev b= case b^.action of                  -- if there is an active action
     Just someAction -> handleBrowserAction ev someAction b -- handle the action 
     Nothing         -> case h of
                          Just _  -> handleHelpEvents ev b
                          Nothing -> handleBrowserNormal ev b            -- else regular events are called
             where h = focusedWindow b ^. help

-- | The event handler, when there is no action or input.
handleBrowserNormal :: Vty.Event -> Browser -> EventM Name Browser
handleBrowserNormal ev = case ev of 
     Vty.EvKey (Vty.KChar 'c')  []  -> browserSetAction Rename "c " False        -- activating the Rename action in the state
     Vty.EvKey (Vty.KChar 't')  []  -> browserSetAction Touch "Touch: " True     -- activating the Touch action in the state
     Vty.EvKey (Vty.KChar 'f')  []  -> browserSetAction TouchF "Make dir: " True -- activating the Touch action in the state
     Vty.EvKey (Vty.KChar 'd')  []  -> browserSetAction Delete "Delete" False    -- activating the Delete action in the state
     Vty.EvKey (Vty.KChar 'm')  []  -> browserSetAction SetMark "Setting mark" False
     Vty.EvKey (Vty.KChar 'y')  []  -> browserSetAction Yank "Yank" False
     Vty.EvKey (Vty.KChar '\'') []  -> browserSetAction Mark "Jump to mark" False
     Vty.EvKey (Vty.KChar 'g')  []  -> browserSetAction Go "Go" False
     Vty.EvKey (Vty.KChar 'o')  []  -> browserSetAction OrderObj "Order" False
     Vty.EvKey (Vty.KChar 'i')  []  -> handleInvert
     Vty.EvKey (Vty.KChar 'n')  []  -> addNewTab 
     Vty.EvKey (Vty.KChar 'r')  []  -> refreshFocusedTab
     Vty.EvKey (Vty.KChar 'x')  []  -> cutSelectedObjects 
     Vty.EvKey (Vty.KChar 'p')  []  -> handleCopy 
     Vty.EvKey (Vty.KChar 'b')  []  -> moveTabBackward 
     Vty.EvKey (Vty.KChar 'w')  []  -> moveTabForward 
     Vty.EvKey (Vty.KChar 'W')  []  -> moveWsForward
     Vty.EvKey (Vty.KChar 'B')  []  -> moveWsBackward
     Vty.EvKey (Vty.KChar 'H')  []  -> handleHSplit 
     Vty.EvKey (Vty.KChar 'V')  []  -> handleVSplit
     Vty.EvKey (Vty.KChar 'h')  []  -> handleChangeDir ev
     Vty.EvKey (Vty.KChar 'l')  []  -> handleChangeDir ev
     Vty.EvKey (Vty.KChar '?')  []  -> handleHelp ev
     Vty.EvKey Vty.KLeft        []  -> handleShiftFocus Tabs.Left
     Vty.EvKey Vty.KRight       []  -> handleShiftFocus Tabs.Right
     Vty.EvKey Vty.KUp          []  -> handleShiftFocus Tabs.Up
     Vty.EvKey Vty.KDown        []  -> handleShiftFocus Tabs.Down
     _                              -> handleOtherEvents ev


-- | This handles the "window" events which mostly are List events
handleOtherEvents :: Vty.Event -> Browser -> EventM Name Browser
handleOtherEvents ev b = do
             w' <- handleWindowEvent ev w    -- Handle non action related event-keys
             let tFresh = refreshTab t w'
             case w'^.windowException of
                 Just e  -> return $ handleIOException e b
                 Nothing -> return $ b & tabs.~(refreshTabZipper ts tFresh)
     where
            w   = focusedWindow b
            ts  = b^.tabs
            t   = focusedTab b

handleHelpEvents :: Vty.Event -> Browser -> EventM Name Browser
handleHelpEvents ev b = case ev of
     Vty.EvKey (Vty.KLeft)      []  -> handleShiftFocus Tabs.Left b
     Vty.EvKey (Vty.KRight)     []  -> handleShiftFocus Tabs.Right b
     Vty.EvKey (Vty.KUp)        []  -> handleShiftFocus Tabs.Up b
     Vty.EvKey (Vty.KDown)      []  -> handleShiftFocus Tabs.Down b
     Vty.EvKey (Vty.KChar '?')  []  -> handleHelp ev b
     _                              ->   do
        w' <- handleWindowHelpEvent ev w
        let tFresh = refreshTab t w'
        case w'^.windowException of
               Just e  -> return $ handleIOException e b
               Nothing -> return $ b & tabs.~ refreshTabZipper ts tFresh
     where
            w   = focusedWindow b
            ts  = b^.tabs
            t   = focusedTab b
--Actions--
-- | Handler for the actions. For every Action value it invokes the corresponding function.
handleBrowserAction ::  Vty.Event -> Action -> Browser -> EventM Name Browser
handleBrowserAction e a =  case a of
     Rename       -> handleBrowserRename e
     RenameObj    -> handleWithInput handleBrowserRenameFile e 
     RenameTab    -> handleWithInput handleBrowserRenameTab e 
     ConfirmPaste -> handleWithInput handleCopyConfirm e 
     Touch        -> handleWithInput handleTouchAction e 
     TouchF       -> handleWithInput handleMakeDir e 
     Paste        -> handleCopyAction 
     Yank         -> handleYank e
     SetMark      -> handleSetMark e
     Mark         -> handleJumpMark e
     Delete       -> handleDelete e
     Go           -> handleGo e
     OrderObj     -> handleOrdObjects e
     _            -> return  

handleChangeDir  :: Vty.Event -> Browser -> EventM Name Browser
handleChangeDir ev b = handleOtherEvents ev b >>= (\b' -> ordFocusedWindow ev b')

handleDelete :: Vty.Event -> Browser -> EventM Name Browser
--handleDelete  (Vty.EvKey( Vty.KChar 'd') [])  = deleteFiles 
handleDelete  (Vty.EvKey( Vty.KChar 'w') [])  = deleteWindow
handleDelete  (Vty.EvKey( Vty.KChar 't') [])  = deleteTab
handleDelete  _                               = return . browserFinishAction ""

-- | Handles the ordering action
handleOrdObjects :: Vty.Event -> Browser -> EventM Name Browser 
handleOrdObjects (Vty.EvKey( Vty.KChar 'n') []) b = b''
        where (ad,_) = b^.order
              b'     = b & order.~(ad,ByName)
              b''    = ordAllWindow $ browserFinishAction "" b'
handleOrdObjects (Vty.EvKey( Vty.KChar 's') []) b = b''
        where (ad,_) = b^.order
              b'     = b & order.~(ad,BySize)
              b''    = ordAllWindow $ browserFinishAction "" b'
handleOrdObjects _ b = b'
        where  b'    = ordAllWindow $ browserFinishAction "" b

handleInvert :: Browser -> EventM Name Browser
handleInvert b = b''
        where (ordering,ordby) = b^.order
              newOrd = case ordering of 
                         Ascending  -> Descending
                         Descending -> Ascending
              b' = b & order.~(newOrd,ordby)
              b'' = ordAllWindow b'

ordAllWindow :: Browser -> EventM Name Browser
ordAllWindow b = return $ b & tabs.~tz'
        where wspaces = b^.tabs
              tz' = fmap (\tz ->              -- fmap Zipper (Zipper Tab)
                      fmap (\t  ->            -- fmap Zipper (Tab)
                              t & renderT.~
                                      (fmap (\w  -> w & -- fmap Tree Direction Window
                                              objects.~(sortObjects (b^.order) (w^.windowName)(w^.objects))) (t^.renderT))
                                & focused.~ 
                                        ((t^.focused) & objects.~(sortObjects (b^.order) (t^.focused.windowName)(t^.focused.objects)))
                           ) tz               -- tz :: Zipper Tab
                         ) wspaces            -- wspaces :: Zipper (Zipper Tab)

ordFocusedWindow :: Vty.Event -> Browser -> EventM Name Browser
ordFocusedWindow (Vty.EvKey( Vty.KChar x) []) b = replaceFocusedWindow w' b
        where w  = focusedWindow b
              t  = focusedTab b
              sorted = sortObjects (b^.order) (w^.windowName) (w^.objects)
              newObjList = if x == 'h' 
                              then
                                    case listSelectedElement (w^.objects) of
                                     Just (_,e) -> listMoveToElement e sorted
                                     Nothing -> listMoveTo 0 sorted
                              else sorted
              w' = w & objects.~newObjList
ordFocusedWindow _ b = return b


-- | Deletes selected files in the focused window.
deleteFiles :: Browser -> EventM Name Browser
deleteFiles b = case obj of 
                   []  -> case selected of 
                           Nothing     -> return b
                           Just (_,a)  -> do
                                   liftIO $ deleteFile a
                                   b' <- refreshFocusedTab b
                                   return $ browserFinishAction "File deleted" b'
                   ls -> do 
                            let msg = "Files " ++ show (length ls) ++ " deleted."
                            liftIO $ mapM_  deleteFile ls
                            b' <- refreshFocusedTab b
                            return $ browserFinishAction msg b'
                            
        where w   = focusedWindow b
              obj = selectedObjects w -- the list of selected objects in focused window
              selected = listSelectedElement (w^.objects)
deleteFile :: Object -> IO ()
deleteFile obj = case obj^.filetype of
                   Directory -> do
                           exists <- doesDirectoryExist (obj^.path)
                           when exists $ removeDirectoryRecursive (obj^.path)
                   _        ->  do
                           exists <- doesFileExist (obj^.path)
                           when exists $ removeFile (obj^.path)

deleteWindow :: Browser -> EventM Name Browser
deleteWindow b = return $ browserFinishAction "" b & tabs.~ refreshTabZipper (b^.tabs) t'
        where t = focusedTab b 
              w = focusedWindow b
              tree = t^.renderT
              tree' = deleteTree w tree
              w'    = getWindow tree'
              ring' = focusSetCurrent (w'^.windowName) (t'^.ring)
              t'    = t & renderT.~tree' & focused.~w' & ring.~ring'

deleteTab :: Browser -> EventM Name Browser
deleteTab b = return $ browserFinishAction "" b & tabs .~ refreshWsZipper (b^.tabs) wspace'
        where wspace  = focusedWSpace b
              wspace' = if emptyp . Z.delete $ wspace 
                           then wspace
                           else Z.delete wspace
              b'      = browserFinishAction "" b

-- | Handler for setting marks. 
handleSetMark :: Vty.Event -> Browser -> EventM Name Browser
handleSetMark (Vty.EvKey( Vty.KChar x) []) b = return $ b' & marks.~m
        where wPath = focusedWindow b ^. currentDir
              m = M.insert x wPath $ b^.marks
              b' = browserFinishAction ("Mark set to " ++ (x : " -> " ++ wPath)) b
handleSetMark _ b = return $ browserFinishAction "Invalid key to set a mark" b

-- | Handler for jumping to marks. 
handleJumpMark :: Vty.Event -> Browser -> EventM Name Browser
handleJumpMark (Vty.EvKey( Vty.KChar x) []) b = case M.lookup x (b^.marks) of
      Nothing -> returnBrowserError ("No mark set to: " ++ [x]) b 
      Just mPath -> do
           exists <- liftIO $ doesDirectoryExist mPath
           if exists then b' else returnBrowserError "Mark path doesn't exists" b
                   where w = focusedWindow b
                         b' = changeWindowDir mPath b
handleJumpMark _ b = return $ browserFinishAction "Invalid key to a mark" b

handleGo :: Vty.Event -> Browser -> EventM Name Browser
handleGo e@(Vty.EvKey( Vty.KChar 'g') []) b = handleOtherEvents e b
handleGo (Vty.EvKey( Vty.KChar 'h') []) b   = 
        do 
        envHome <- liftIO $ lookupEnv "HOME"  
        case envHome of
           Nothing   -> returnBrowserError "Couldn't find Home dir" b
           Just path -> changeWindowDir path b
handleGo (Vty.EvKey( Vty.KChar '/') []) b = changeWindowDir "/" b
handleGo _ b = return . browserFinishAction "" $ b

getSize :: Object -> Integer
getSize Object {_info = Nothing}   = 0
getSize Object {_info = (Just i)}  = i^.size

toLowercase :: String -> String
toLowercase = fmap toLower 
sortPartitions :: (Ordering',OrderBy) -> ([Object], [Object]) -> [Object]
sortPartitions (Ascending, ByName) (dirs,files)  = 
       sortBy (\n m -> compare (toLowercase $ n^.name) (toLowercase $ m^.name)) dirs ++
       sortBy (\n m -> compare (toLowercase $ n^.name) (toLowercase $ m^.name)) files
sortPartitions (Ascending, BySize) (dirs,files)  =
       sortBy (\n m -> compare (getSize n) (getSize m)) dirs ++
       sortBy (\n m -> compare (getSize n) (getSize m)) files
sortPartitions (Descending, BySize) (dirs,files)  =
        (DL.reverse $ sortBy (\n m -> compare (getSize n) (getSize m)) dirs) ++
        (DL.reverse $ sortBy (\n m -> compare (getSize n) (getSize m)) files)
sortPartitions (Descending,ByName) (dirs,files)  = 
        (DL.reverse $ sortBy (\n m -> compare (toLowercase $ n^.name) (toLowercase $ m^.name)) dirs) ++
        (DL.reverse $ sortBy (\n m -> compare (toLowercase $ n^.name) (toLowercase $ m^.name)) files)

sortObjects :: (Ordering',OrderBy) -> Name -> List Name Object -> List Name Object
sortObjects ordering name' l = list name' newV 1
         where newV =(V.fromList . sortPartitions ordering
                    . partition (\n -> n^.filetype == Directory) . V.toList . listElements) $ l

changeWindowDir :: FilePath -> Browser -> EventM Name Browser
changeWindowDir path b = do 
                exists <- liftIO $ doesDirectoryExist path
                if exists then b' else returnBrowserError "Path doesn't exists" b
                where w  = focusedWindow b
                      b' = do
                       w' <- liftIO $ changeDir path w
                       let ordObjects' = sortObjects (b^.order) (w'^.windowName) (w'^.objects) 
                       let w'' = w' & objects.~ordObjects'
                       b'' <- return $ browserFinishAction "" b
                       replaceFocusedWindow w'' b''

-- | This function is to handle input for events, for example: touch, rename, make dir. 
handleWithInput :: (Vty.Event -> Browser -> EventM Name Browser) -- ^ The handler function to execute after user input.
                -> Vty.Event                                   
                -> Browser
                -> EventM Name Browser
handleWithInput f e b = if b^.inputMode then handleBrowserInput e b else f e b

-- Rename and Touch -- 

browserSetAction :: Action ->  String -> Bool -> Browser -> EventM Name Browser 
browserSetAction a msg f b= return $ b & action?~a 
                            & inputMode.~f & statusLine.~msg & input.~"" 

handleBrowserRename :: Vty.Event -> Browser -> EventM Name Browser 
handleBrowserRename e@(Vty.EvKey( Vty.KChar 'c') []) b = browserSetAction RenameObj "Rename: " True b
handleBrowserRename e@(Vty.EvKey( Vty.KChar 't') []) b = browserSetAction RenameTab "Rename tab: " True b
handleBrowserRename _ b = returnBrowserError "" b


handleBrowserRenameFile :: Vty.Event -> Browser -> EventM Name Browser 
handleBrowserRenameFile _ b = 
     case oldM of
         Just old  -> case old^.filetype of 
                        Directory ->if isValid new && notExists
                                     then do                             
                                       liftIO $ renameDirectory (old^.name) new
                                       let b' = browserFinishAction ("Success new name is: " ++ new) b
                                       refreshFocusedTab b' 
                                     else returnBrowserError "Invalid name" b
                        _         -> if isValid new && notExists
                                     then do                             
                                       liftIO $ renameFile (old^.name) new
                                       let b' = browserFinishAction ("Success new name is: " ++ new) b
                                       refreshFocusedTab b' 
                                     else returnBrowserError "Invalid name" b
         Nothing -> returnBrowserError "Something went wrong while renaming" b
     where 
        new = b^.input
        w   = focusedWindow b
        objNames = foldr (\n l -> (n^.name) : l) [] (w^.objects)
        notExists = not $ new `elem` objNames
        oldM :: Maybe Object
        oldM = do
           (_,item) <- listSelectedElement ((focusedWindow b)^.objects)
           return $ item

handleBrowserRenameTab :: Vty.Event -> Browser -> EventM Name Browser
handleBrowserRenameTab _ b = return $ b' & tabs.~tz'
        where b' = browserFinishAction "" b
              t  = focusedTab b
              tz = focusedWSpace b
              t' = t & tabName.~makeNewTabName b 0 (b^.input)  
              tz' = Z.replace (Z.replace t' tz) (b^.tabs)
--Copy--
-- | The main copy handler
handleCopy :: Browser -> EventM Name Browser
handleCopy b = do 
        b' <- browserSetAction Paste ("Copying: " ++ obj ++ ", please wait") False b
        handleCopyAction b'
        where buffr = fromBoard $ b^.buffer
              obj = if buffr /= []
                       then (head buffr)^.name
                       else ""
-- | Handler to yank files from the tab
handleYank :: Vty.Event -> Browser -> EventM Name Browser
handleYank e = case e of 
      Vty.EvKey (Vty.KChar 'y') [] -> copySelectedObjects 
      Vty.EvKey (Vty.KChar 'a') [] -> copyCurrDir
      Vty.EvKey (Vty.KChar 'i') [] -> undefined --  copyCurrDirFiles
      Vty.EvKey (Vty.KChar 't') [] -> copyTab
      _                            -> return . browserFinishAction "" 

-- | Copies all the selected objects from the tab. Even the not focused window's selected objects makes it to the clipboard.
copyTab :: Browser -> EventM Name Browser
copyTab b = do 
        let b' = browserFinishAction ("Copied " ++ (show . length $ l) ++ " files from this tab") b
        return $ b' & clipboard.~clip & buffer.~clip
        where tree = (focusedTab b)^.renderT
              l    = nub . concatMap selectedObjects . foldr (:) [] $ tree
              clip = Clipboard l

copyMessage :: Object -> String
copyMessage obj = case obj^.filetype of 
                    Directory ->  obj^.name ++ " directory already exists, do you want to replace everything inside it?(y/n)"
                    _         ->  obj^.name ++ " already exists, do you want to replace it?(y/n)"

handleCopyAction :: Browser -> EventM Name Browser
handleCopyAction b@Browser {_clipboard=Browser.Empty} = returnBrowserError "Clipboard is empty" b
handleCopyAction b@Browser {_buffer=Clipboard []} = do
        _ <- liftIO $ refreshFocusedW t w
        b' <- refreshFocusedTab . browserReset $ b
        return $ b' & statusLine.~"Copy complete" & buffer.~clip -- resets the buffer from the clipboard
                where t  = focusedTab b
                      w  = focusedWindow b
                      clip = b^.clipboard
handleCopyAction b@Browser {_buffer=Cutboard []} = do  --the same but we set the clipboard to empty 
        _ <- liftIO $ refreshFocusedW t w
        b' <- refreshFocusedTab . browserReset $ b
        return $ b' & statusLine.~"Copy complete" 
                    & buffer.~Browser.Empty
                    & clipboard.~Browser.Empty         -- flush the clipboard
                where t  = focusedTab b
                      w  = focusedWindow b
handleCopyAction b = do
        contents <- liftIO $ getDirectoryContents $ w^.currentDir
        if any (\n -> n == x^.name) contents
           then browserSetAction ConfirmPaste msg True b 
           else pasteOneObject False b
        where 
              (x:_) = fromBoard $  b^.buffer
              w      = focusedWindow b
              msg    = copyMessage x

-- | Calls safeCopyPaste or safeCutPaste depending on the clipboard type. If the file exists in the destination directory, 
-- it sets the HandlePasteConfirm action in the state. Basically it asks the user to overwrite the file or not. 
pasteOneObject :: Bool  -- ^ the bool indicates if the file has been confirmed to overwrite. 
               -> Browser
               -> EventM Name Browser 
pasteOneObject _ b@Browser {_buffer=Browser.Empty} = returnBrowserError "Clipboard is empty" b
pasteOneObject c b@Browser {_buffer=Clipboard (x:_)} = do
        contents <- liftIO $ getDirectoryContents $ w^.currentDir
        case c of 
          True -> safeCopyPaste b
          False -> if any (\n -> n == x^.name) contents
           then browserSetAction ConfirmPaste msg True b 
           else safeCopyPaste b
        where 
              w      = focusedWindow b
              msg    = copyMessage x

pasteOneObject c b@Browser {_buffer=Cutboard (x:_)} = do
        contents <- liftIO $ getDirectoryContents $ w^.currentDir
        case c of 
          True -> safeCutPaste b
          False -> if any (\n -> n == x^.name) contents
                   then browserSetAction ConfirmPaste msg True b 
                   else safeCutPaste b
         where
             w      = focusedWindow b
             msg    = copyMessage x
pasteOneObject _ b = returnBrowserError "Clipboard is empty" b

-- | This function pastes the file from the clipboard and deletes it from it's original location.
-- If an error occurs in the the copying process, the function won't delete the original file. 
safeCutPaste :: Browser -> EventM Name Browser
safeCutPaste b@Browser {_buffer=Browser.Empty} = returnBrowserError "Clipoard is empty" b
safeCutPaste b@Browser {_buffer=Cutboard (x:xs)} = case (x^.filetype) of 
       Directory -> do
         t <- liftIO  tryPasteDir
         handlePasteDir t
       _         ->  do 
         t <- liftIO  tryPasteFile 
         handlePasteFile t
  where
          w      = focusedWindow b
          tryPasteDir :: IO (Either SomeException ())
          tryPasteDir   = try $ PR.copyDirRecursive (pack $ x^.path) (pack $ w^.currentDir </> x^.name) PR.Overwrite PR.FailEarly
          tryPasteFile :: IO (Either SomeException ())
          tryPasteFile  = try $ copyFile (x^.path) $ w^.currentDir </> x^.name
          handlePasteDir t  = either 
                (\_ -> return $ browserFinishAction ("Error while copying dir: " ++ x^.name ) b)
                (\_ -> do liftIO $ removeDirectoryRecursive (x^.path)
                          b' <- browserSetAction Paste ("Copied " ++ x^.name) False b 
                           -- Setting action to Paste, to continue the copying process
                           -- the message actually never shows
                          handleCopyAction $ b' & buffer.~(Cutboard xs)) t
                           -- continuing the copy process
          handlePasteFile t = either 
                (\_ -> return $ browserFinishAction ("Error while copying file: " ++ x^.name) b)
                (\_ -> do liftIO $ removeFile (x^.path)
                          b' <- browserSetAction Paste ("Copied " ++ x^.name) False b
                          handleCopyAction $ b' & buffer.~(Cutboard xs)) t
safeCutPaste b = returnBrowserError "Something went wrong (safeCutPaste)" b

-- | Pastes the files from the clipboard. If error occurs the process is stopped.
safeCopyPaste :: Browser -> EventM Name Browser 
safeCopyPaste  b@Browser {_buffer=Browser.Empty} = returnBrowserError "Clipboard is empty" b
safeCopyPaste  b@Browser {_buffer=Clipboard (x:xs)} = do 
        case (x^.filetype) of 
            Directory -> do
                t   <- liftIO tryPasteDir
                handlePasteDir t
            _         ->  do
                t <- liftIO tryPasteFile
                handlePasteFile t
        where
          w = focusedWindow b
          tryPasteDir :: IO (Either SomeException ())
          tryPasteDir   = try $ PR.copyDirRecursive (pack $ x^.path) (pack $ w^.currentDir </> x^.name) PR.Overwrite PR.FailEarly
          tryPasteFile :: IO (Either SomeException ())
          tryPasteFile  = try $ copyFile (x^.path) $ w^.currentDir </> x^.name
          handlePasteDir t  = either 
                             (\_ -> return $ browserFinishAction ("Error while copying dir: " ++ x^.name ) b)
                             (\_ -> do 
                                       b' <- browserSetAction Paste ("Copied " ++ x^.name) False b
                                       handleCopyAction $ b' & buffer.~Clipboard xs) t
          handlePasteFile t = either 
                                   (\_ -> return $ browserFinishAction ("Error while copying file: " ++ x^.name) b)
                                   (\_ -> do 
                                             b' <- browserSetAction Paste ("Copied " ++ x^.name) False b
                                             handleCopyAction $ b' & buffer.~(Clipboard xs)) t
safeCopyPaste b = returnBrowserError "Something went wrong (safeCutPaste)" b

-- | This function asks the user's permission to overwrite the file or directory.
handleCopyConfirm :: Vty.Event -> Browser -> EventM Name Browser
handleCopyConfirm _ b@Browser {_buffer=Browser.Empty} = return $ browserFinishAction "Clipboard is empty" b & buffer.~Browser.Empty
handleCopyConfirm _ b@Browser {_buffer=Clipboard []}  = return $ browserFinishAction "Clipboard is empty" b & buffer.~Browser.Empty
handleCopyConfirm _ b@Browser {_buffer=Cutboard  []}  = return $ browserFinishAction "Clipboard is empty" b & clipboard.~Browser.Empty & buffer.~Browser.Empty
handleCopyConfirm _ b@Browser {_buffer=Clipboard  x}
  | map toLower (b^.input) == "y" = pasteOneObject True b --should use some parser
  | map toLower (b^.input) == "n" = handleSkipCopy x b 
  | otherwise                     = handleSkipCopy x b -- if the user's answer is not recognizable it skips the file. 

handleCopyConfirm _ b@Browser {_buffer=Cutboard x}
  | map toLower (b^.input) == "y" = pasteOneObject True b
  | map toLower (b^.input) == "n" = handleSkipCopy x b 
  | otherwise                     = handleSkipCopy x b

-- | This function is deprecated        
handleReplaceCopy :: [Object] ->  Browser -> EventM Name Browser
handleReplaceCopy [] b = returnBrowserError "Clip is empty when replacing file" b
handleReplaceCopy (x:xs) b = do 
        case (x^.filetype) of 
            Directory -> liftIO $ PR.copyDirRecursive (pack $ x^.path)
                                                      (pack $ w^.currentDir </> x^.name)
                                                      PR.Overwrite
                                                      PR.FailEarly
            _ -> liftIO $ copyFile (x^.path) $ w^.currentDir </> x^.name
        b'' <- b'
        handleCopyAction $ b'' & buffer.~Clipboard xs
                where
                   b' = browserSetAction Paste "" False b
                   w  = focusedWindow b

-- | Skips one object from the clipboard
handleSkipCopy :: [Object] -> Browser -> EventM Name Browser
handleSkipCopy [] b = returnBrowserError "Clip is empty when skipping file" b
handleSkipCopy (_:xs) b = do
        b'' <- b'
        handleCopyAction $ b'' & buffer.~Clipboard xs
        where b' = browserSetAction Paste "" False b

-- | Gets the currently focused window's selected object.
getSelectedElement :: Browser -> EventM Name Browser
getSelectedElement b = case listSelectedElement (w^.objects) of 
        Just (_,obj) -> return $ b & buffer.~Clipboard [obj] 
                                   & clipboard.~Clipboard [obj]
                                   & action.~Nothing
                                   & statusLine.~ (obj^.name) ++ " copied to clipoard."
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

--Input--

-- | Handles the user's input
handleBrowserInput :: Vty.Event ->  Browser -> EventM Name Browser  
handleBrowserInput ev b = 
 case ev of
     Vty.EvKey Vty.KEsc         []  -> return $ browserReset b
     Vty.EvKey Vty.KEnter       []  -> handleBrowserEvent ev $ b & input.~inputS & inputMode.~False
     _                              -> do
            newEditor <- handleEditorEvent ev (b^.bEditor)
            return $ b & bEditor.~newEditor
 where
     inputS :: String
     inputS = concat $ getEditContents $ b^.bEditor


handleTouchAction :: Vty.Event -> Browser -> EventM Name Browser
handleTouchAction _  = handleTouch 

-- | Handles the creation of an empty file
handleTouch :: Browser -> EventM Name Browser
handleTouch b = if isValid (b^.input) 
                  then do 
                          files <- liftIO $ getDirectoryContents (w^.currentDir) 
                          if (b^.input) `elem` files 
                             then returnBrowserError "File already exists" b
                             else do         
                                   tmp <- liftIO $ openTempFile dir (b^.input)
                                   _ <- liftIO $ renameFile (fst tmp) (b^.input)
                                   _ <- liftIO $ refreshFocusedW t w 
                                   let b'' = b' & tabs.~(refreshTabZipper tz t)
                                   refreshFocusedTab b''       
                  else returnBrowserError "Invalid filename" b
        where
                w    = focusedWindow b
                t    = focusedTab b
                tz   = b^.tabs
                dir  = w^.currentDir
                b'   = browserFinishAction (b^.input ++ " created with read and write permissions") b

-- | Handles the creation of an empty directory 
handleMakeDir :: Vty.Event -> Browser -> EventM Name Browser
handleMakeDir _ b= if isValid (b^.input) 
                  then do 
                          files <- liftIO $ getDirectoryContents (w^.currentDir) 
                          if (b^.input) `elem` files 
                             then returnBrowserError "Directory already exists" b
                             else do         
                                   _ <- liftIO $ createDirectory (b^.input)
                                   _ <- liftIO $ refreshFocusedW t w 
                                   let b'' = b' & tabs.~(refreshTabZipper tz t)
                                   refreshFocusedTab b''       
                  else returnBrowserError "Invalid directory name" b
        where
                w    = focusedWindow b
                t    = focusedTab b
                tz   = b^.tabs
                dir  = w^.currentDir
                b'   = browserFinishAction (b^.input ++ " created with read and write permissions") b


--Tab and window operations--                       
refreshTabZipper :: Zipper (Zipper(Tab)) -> Tab -> Zipper( Zipper(Tab))
refreshTabZipper z t = (Z.insert newZipper $ Z.delete z)
        where
       tZipper = cursor z
       newZipper = (Z.insert t $ Z.delete tZipper)
refreshWsZipper :: Zipper (Zipper Tab) -> Zipper Tab -> Zipper (Zipper Tab)
refreshWsZipper zz z = Z.insert z $ Z.delete zz

-- | Insert tab into workspace 
insertIntoWs :: Zipper Tab -> Browser -> Browser
insertIntoWs tz b = b & tabs.~(Z.insert tz (b^.tabs))

focusedWSpace :: Browser -> Zipper(Tab)
focusedWSpace b = cursor $ b^.tabs 

-- | Splitting window horizontally
handleHSplit :: Browser -> EventM Name Browser
handleHSplit b = do 
        w' <- liftIO $ newWindow (WindowName ((b^.winCount) + 1)) "."
        let newT = hSplitWindow (focusedTab b) w' Tabs.Horizontal
        return $ b & tabs.~(refreshTabZipper (b^.tabs) newT) & winCount.~((b^.winCount) + 1)
               
-- | Splitting window vertically
handleVSplit :: Browser -> EventM Name Browser
handleVSplit b = do 
        w' <- liftIO $ newWindow (WindowName ((b^.winCount) + 1)) "."
        let newT = hSplitWindow (focusedTab b) w' Tabs.Vertical
        return $ b & tabs.~(refreshTabZipper (b^.tabs) newT) & winCount.~((b^.winCount) + 1)

makeNewTabName :: Browser -> Int -> String -> Name
makeNewTabName b c tabname  = if elem newName names 
                                    then makeNewTabName b (c+1) tabname
                                    else newName
        where ts      = concat . map (\n -> Z.toList n) $ Z.toList (b^.tabs)
              names   = getTabNames ts
              newName = case c of 
                         0 -> TabName tabname
                         _ -> TabName (tabname ++ " (" ++ show c ++ ")")

moveTabForward :: Browser -> EventM Name Browser 
moveTabForward b = return $ b & tabs.~newWs
                where
                        w = focusedWSpace b :: Zipper Tab
                        z = if endp (right w) then start w else right w :: Zipper Tab
                        newWs = refreshWsZipper (b^.tabs) z

moveTabBackward :: Browser -> EventM Name Browser 
moveTabBackward b = return $ b & tabs.~newWs
                where
                        w = focusedWSpace b :: Zipper Tab
                        z = if beginp w then left (end w) else left w 
                        newWs = refreshWsZipper (b^.tabs) z

moveWsForward :: Browser -> EventM Name Browser 
moveWsForward b = return $ b & tabs.~z & wspace.~n
                where
                        w = b^.tabs :: Zipper (Zipper Tab)
                        (z,n) = if endp (right w) then (start w, 1)
                                                  else (right w, (b^.wspace)+ 1)  :: (Zipper (Zipper Tab),Int)

moveWsBackward :: Browser -> EventM Name Browser 
moveWsBackward b = return $ b & tabs.~z & wspace.~n
                where
                        w = b^.tabs :: Zipper (Zipper Tab)
                        (z,n) = if beginp w then (left (end w),3) 
                                            else (left w, (b^.wspace) - 1) :: ((Zipper (Zipper Tab)),Int)

-- | This function handles the movement between windows
handleShiftFocus :: Movement -> Browser -> EventM Name Browser
handleShiftFocus m b = return $ b & tabs.~t'
        where t' = refreshTabZipper (b^.tabs) (shiftFocus m $ focusedTab b)

refreshFocusedTab :: Browser -> EventM Name Browser
refreshFocusedTab b@(Browser {_tabs = tz}) = do 
      t' <- liftIO $ refreshFocusedW t (t^.focused)
      let tz' = refreshTabZipper tz t' 
      let b' = b & tabs.~tz'
      ordAllWindow b' 
      where
         t = focusedTab b
         
getTabNames :: [Tab] -> [Name]
getTabNames []                       = []
getTabNames [(Tab {_tabName=n}) ]    = [n]
getTabNames ((Tab {_tabName=n}):xs)  = n : getTabNames xs

addNewTab :: Browser -> EventM Name Browser 
addNewTab b = do
        win <- liftIO $ newWindow (WindowName $ b^.winCount + 1) "."
        dirName <- liftIO $ getCurrentDirectory
        let n = makeNewTabName b 0 dirName -- make a unique tab name
        let t = newTab n win
        let newWs = Z.insert (Z.insert t (cursor $ b^.tabs)) $ Z.delete (b^.tabs)
        return $ b & tabs.~newWs & winCount.~count
                where
                        w     = focusedWindow b
                        count = b^.winCount + 1

focusedWindow :: Browser -> Window
focusedWindow b = t^.focused
        where t = focusedTab b

focusedTab :: Browser -> Tab
focusedTab b = cursor . cursor $ b^.tabs

getTabName :: Tab -> String
getTabName t =(\(TabName s) -> s) $ t^.tabName

fTabName :: [String] -> Zipper Tab -> [String]
fTabName x t = s : x
        where s = getTabName $ cursor t

replaceFocusedWindow :: Window -> Browser -> EventM Name Browser
replaceFocusedWindow w b = if w == wFocused then return $ b & tabs.~tz
                                            else return b
        where t = focusedTab b
              wFocused = focusedWindow b
              tree = replaceInTree w (t^.renderT)
              t' = t & focused.~w & renderT.~tree
              tz = refreshTabZipper (b^.tabs) t'



collectTabNames :: Browser ->  [String]
collectTabNames b = concat . Z.toList $ fmap (\n -> foldlz fTabName [] n) t
        where t = start $ b^.tabs
        
collectTabNamesFocused :: Browser -> [String]
collectTabNamesFocused b = foldlz fTabName [] (start $ focusedWSpace b)

                               
--- Utility ---

browserFinishAction :: String   -- ^ the message to be written to the statusline
                    -> Browser  -- ^ current state
                    -> Browser 
browserFinishAction message b = b & 
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
     case e of
        permissionErrorType   -> browser & statusLine .~ "Permission error"
        doesNotExistErrorType -> browser & statusLine .~"Directory or file doesn't exists"
        _     -> browser & statusLine .~ "Unexpected error"
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
returnBrowserError e b = return $
        b & inputMode.~False 
          & statusLine.~e 
          & action.~Nothing 
          & input.~""  
          & bEditor.~emptyEditor

selectedObjects :: Window -> [Object]
selectedObjects window = filter (^.isSelected) l
        where
                l = V.toList . listElements $ window^.objects 

ordObjects :: Window -> List Name Object
ordObjects window = list resourceName (V.fromList $ sort l) 1
        where
                l = V.toList . listElements $ window^.objects 
                resourceName  = window^.windowName

helpText' :: [String]
helpText' = [ " h  to move to the parent directory",
             " l  to move into a directory",
             " j  to move down in the list",
             " k  to move up in the list",
             " gg to move to the beginning of the list",
             " G to move to the end of the list",
             " Ctrl-d to scroll half page down",
             " Ctrl-u to scroll half page up",
             " Ctrl-b to scroll one page down",
             " Ctrl-f to scroll one page up",
             " Ctrl-f to scroll one page up",
             " H  to split window horizontally",
             " V  to split window vertically",
             " n  to add new tab",
             " w  to shift the tab focus right",
             " b  to shift the tab focus left",
             " B  to change workplace backward",
             " W  to change workplace forward",
             " r  to refresh tab",
             " t  to create an empty file",
             " f  to create an empty folder",
             " i  to invert order",
             " m  to set mark",
             " \'  to jump to a mark",
             " dd  to delete selected a items",
             " dt  to delete a tab",
             " dw  to delete a window",
             " cc  to rename a file or directory",
             " ct  to rename focused tab",
             " gh  to move to home directory",
             " gr  to move to root directory",
             " s   to select items",
             " x   to cut selected items",
             " yy  to copy selected items",
             " ya  to copy directory contents",
             " yt  to copy focused tab's selected items ",
             " p   to paste files/dirs",
             " on  to order by name",
             " os  to order by size",
             " Use arrows to navigate between windows "
           ]
helpList :: Name -> List Name String
helpList n = L.list n (V.fromList helpText') 1

handleHelp :: Vty.Event -> Browser -> EventM Name Browser
handleHelp e b = case w^.help of
                   Nothing -> return b'
                   Just a -> return bUnHelp
     where w = focusedWindow b
           t = focusedTab b
           wHelp = w & help ?~ helpList (WindowName (b^.winCount+1))
           t' = refreshTab t wHelp
           b' = b & tabs.~refreshTabZipper (b^.tabs) t' & winCount.~(b^.winCount) + 1
           wUnHelp  = w & help .~ Nothing
           tUnHelp = refreshTab t wUnHelp
           bUnHelp = b & tabs.~(refreshTabZipper (b^.tabs) tUnHelp) & winCount.~((b^.winCount) + 1)
        
tEmpty :: A.AttrName
tEmpty = A.attrName "tempty"

tFocused :: A.AttrName
tFocused = A.attrName "tfocused"

attrFill :: A.AttrName
attrFill = A.attrName "fill"
