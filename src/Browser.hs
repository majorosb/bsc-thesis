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
import Brick.Widgets.Core

data Action  = Rename | Move deriving Show

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

makeLenses ''Browser

getFocusedWindow :: Browser -> Window
getFocusedWindow b = t^.focused
        where t = cursor $ b^.tabs

statusLineW :: String -> Widget a
statusLineW s = vLimit 1 $ str s

renderBrowser ::  Browser -> Widget Name
renderBrowser browser = 
        vBox [ renderTab (cursor $ browser^.tabs)
             , statusline
             ] where
                   statusline = if browser^.inputMode 
                       then statusLineW (( show . fromJust $ browser^.action)++ ": ")<+> 
                               renderEditor (str . unlines) True (browser^.bEditor)
                       else statusLineW sLine
                   sLine = getStatusLine browser                                
--                        showCursor 
--                        (browser^.browserName)
--                        (Location (length $ browser^.input,0))
--                        (str $ browser^.input)

-- Relationship between browser events are the following:
-- If the Browser has an active Action (Just someAction) then its trying to complete that action
-- until it returns Nothing. handleBrowserEvent will pass the current state and the event parameters to
-- handleBrowserAction. 
--

newBrowser :: Name -> Tab ->  IO (Browser )
newBrowser name tab = return $ Browser (fromList $ tab:[]) name "" "" ed Nothing False Browser.Empty 1
        where ed = editor (EditName "editor")  (Just 1) ""

--- Events ---
handleBrowserEvent :: Vty.Event -> Browser -> EventM Name (Browser )
handleBrowserEvent ev browser = case browser^.action of              -- if there is an active action
     Just someAction -> handleBrowserAction ev someAction browser    -- handle the action 
     Nothing         -> handleBrowserNormal ev browser               -- else regular window events are called

handleBrowserNormal :: Vty.Event -> Browser -> EventM Name (Browser ) 
handleBrowserNormal ev browser = case ev of 
     Vty.EvKey (Vty.KChar 'r')  []  -> return $ browserSetAction Rename browser  -- activating the Rename action in the state
     Vty.EvKey (Vty.KChar 't')  []  -> addNewTab browser
     Vty.EvKey (Vty.KChar 'y')  []  -> copySelectedObjects browser 
     Vty.EvKey (Vty.KChar 'b')  []  -> moveTabBackward browser
     Vty.EvKey (Vty.KChar 'w')  []  -> moveTabForward browser
     Vty.EvKey (Vty.KChar 'o')  []  -> handleOrdObjects browser
     Vty.EvKey (Vty.KChar 'H')  []  -> handleHSplit browser 
     Vty.EvKey (Vty.KChar 'V')  []  -> handleVSplit browser 
     otherwise                      -> do 
             newW <- handleWindowEvent ev w                    -- Handle non action related event-keys
             case (newW^.windowException) of
                 Just e  -> return $ handleIOException e browser 
                 Nothing -> return $ browser & tabs.~(Z.insert (refreshTab t (newW ,Tabs.Horizontal) ) $ Z.delete ts)
     where
            w  = getFocusedWindow browser
            ts = browser^.tabs
            t  = cursor ts

refreshTabZipper :: Zipper(Tab) -> Tab -> Zipper(Tab)
refreshTabZipper z t = (Z.insert t $ Z.delete z)

currentTab :: Browser -> Tab
currentTab b = cursor $ b^.tabs

handleHSplit :: Browser -> EventM Name Browser
handleHSplit b = do 
        newW <- liftIO $ newWindow (WindowName ((b^.winCount) + 1)) "."
        let newT = hSplitWindow (currentTab b) newW Tabs.Horizontal
        return $ b & tabs.~(refreshTabZipper (b^.tabs) newT) & winCount.~((b^.winCount) + 1)
               
handleVSplit :: Browser -> EventM Name Browser
handleVSplit b = do 
        newW <- liftIO $ newWindow (WindowName ((b^.winCount) + 1)) "."
        let newT = hSplitWindow (currentTab b) newW Tabs.Vertical
        return $ b & tabs.~(refreshTabZipper (b^.tabs) newT) & winCount.~((b^.winCount) + 1)

makeNewTabName :: Browser -> Int -> String -> Name
makeNewTabName browser c name  = if elem newName names then makeNewTabName browser (c+1) name
                                                  else case c of 
                                                         1 -> TabName name
                                                         _ -> TabName (name ++ " (" ++ show c ++ ")")
        where ts      = Z.toList (browser^.tabs)
              names   = getTabNames ts
              newName = TabName name 
       -- rewrite it with has

handleBrowserAction ::  Vty.Event -> Action -> Browser -> EventM Name (Browser )
handleBrowserAction e a b =  case a of
     Rename -> newBrowserState e a b
     _      -> return b 
 where
     newBrowserState e a b = handleBrowserInput e a b 
     mode :: Bool
     mode = b^.inputMode

handleBrowserInput ::  Vty.Event -> Action -> Browser -> EventM Name Browser  
handleBrowserInput ev a browser = 
 case ev of
     Vty.EvKey Vty.KEsc         []  -> return $ browserRevertAction browser
     Vty.EvKey Vty.KEnter       []  -> handleBrowserRename (browser & input.~inputS)
     _                              -> do
            newEditor <- handleEditorEvent ev (browser^.bEditor)
            return $ browser & bEditor.~newEditor
 where
     inputS :: String
     inputS = concat $ getEditContents $ browser^.bEditor


handleOrdObjects :: Browser -> EventM Name Browser 
handleOrdObjects browser = return $ browser & tabs.~(refreshTabZipper tz newTab)
        where w      = getFocusedWindow browser
              newW   = w & objects.~ordObjects w
              t      = cursor $ browser^.tabs
              newTab = t & focused.~newW
              tz     = browser^.tabs


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
           (_,item) <- listSelectedElement ((getFocusedWindow browser)^.objects)
           return $ item^.name


getTabNames :: [Tab] -> [Name]
getTabNames [(Tab n _ _ _ ) ]    = [n]
getTabNames ((Tab n _ _ _ ):xs)  = n : getTabNames xs

addNewTab :: Browser -> EventM Name Browser 
addNewTab browser = do
        win <- liftIO $ newWindow (WindowName $ browser^.winCount + 1) "."
        dirName <- liftIO $ Dir.getCurrentDirectory
        let n = makeNewTabName browser 1 dirName
        let t = newTab n win
        return $ browser & tabs.~(Z.insert t (browser^.tabs)) & winCount.~count
                where
                        w     = (cursor $ browser^.tabs)^.focused
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

copySelectedObjects :: Browser -> EventM Name (Browser ) 
copySelectedObjects browser =return $ browser & clipboard.~clip
        where clip = Clipboard $ selectedObjects (win^.focused)
              win  = cursor $ browser^.tabs

--- Utility ---

browserSetAction :: Action -> Browser -> Browser 
browserSetAction a browser = browser & action.~(Just a) & inputMode.~True & statusLine.~"" & input.~"" 

browserFinishAction :: String       ->    -- the message to be written to the statusline
                       Browser      ->    -- current state
                       Browser 
browserFinishAction message browser = browser & 
     inputMode.~False     &               -- disabling inputMode
     statusLine.~message  & 
     action.~Nothing      &
     bEditor.~editor (EditName "e") (Just 1) ""        -- resetting the editor
                         

browserRevertAction :: Browser -> Browser 
browserRevertAction browser = browser &
     action.~Nothing  & 
     inputMode.~False &
     statusLine.~""   &
     input.~"" 

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
