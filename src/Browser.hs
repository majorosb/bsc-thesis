{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Browser
        where   
import qualified Graphics.Vty as Vty
import qualified Control.Exception as E
import qualified Data.Vector as V
import qualified System.Directory as Dir

import Object 
import Window
import Control.Lens
import Control.Monad.IO.Class
import System.IO.Error
import System.FilePath.Posix
import Data.Maybe
import Data.Char

import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.List
import Brick.Widgets.Edit
import Brick.Widgets.Core

data Action  = Rename | Move deriving Show

data Browser a = Browser { _window      :: Window a
                         , _browserName :: a
                         , _statusLine  :: String 
                         , _input       :: String 
                         , _bEditor     :: Editor String a
                         , _action      :: Maybe Action
                         , _inputMode   :: Bool
                         }
makeLenses ''Browser



statusLineW :: String -> Widget a
statusLineW s = vLimit 1 $ str s


renderBrowser :: (Ord a, Show a) => Browser a -> Widget a
renderBrowser browser = 
        vBox [ renderWindow (browser^.window)
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

newBrowser :: (Num a, Ord a) => a ->  Window a  ->  IO (Browser a)
newBrowser name window = return $ Browser window name "" "" ed Nothing False
        where ed = editor 3 (Just 1) ""

--- Events ---
handleBrowserEvent :: (Ord a, Num a) => Vty.Event -> Browser a -> EventM a (Browser a)
handleBrowserEvent ev browser = case browser^.action of              -- if there is an active action
     Just someAction -> handleBrowserAction ev someAction browser    -- handle the action 
     Nothing         -> handleBrowserNormal ev browser               -- else regular window events are called

handleBrowserNormal :: (Ord a, Num a) => Vty.Event -> Browser a -> EventM a (Browser a) 
handleBrowserNormal ev browser = case ev of 
     Vty.EvKey (Vty.KChar 'r')  []  -> return $ browserSetAction Rename browser  -- activating the Rename action in the state
     otherwise                      -> do 
             newWindow <- handleWindowEvent ev (browser^.window)                 -- Handle non action related event-keys
             case (newWindow^.windowException) of
                 Just e  -> return $ handleIOException e browser 
                 Nothing -> return $ browser & window .~ newWindow 

handleBrowserAction :: (Ord a,Num a) => Vty.Event -> Action -> Browser a -> EventM a (Browser a)
handleBrowserAction e a b =  case a of
     Rename -> newBrowserState e a b
     _      -> return b 
 where
     newBrowserState e a b = handleBrowserInput e a b 
     mode :: Bool
     mode = b^.inputMode

handleBrowserInput :: (Ord a, Num a) => Vty.Event -> Action -> Browser a -> EventM a (Browser a) 
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



handleBrowserRename :: (Ord a, Num a) =>  Browser a -> EventM a (Browser a)
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
           (_,item) <- listSelectedElement (browser^.window^.objects)
           return $ item^.name


--- Utility ---

browserSetAction :: Action -> Browser a -> Browser a
browserSetAction a browser = browser & action.~(Just a) & inputMode.~True & statusLine.~"" & input.~"" 

browserFinishAction :: (Ord a, Num a) => 
                       String    ->                       -- the message to be written to the statusline
                       Browser a ->                       -- current state
                       Browser a 
browserFinishAction message browser = browser & 
     inputMode.~False     &           -- disabling inputMode
     statusLine.~message  & 
     action.~Nothing      &
     bEditor.~(editor 3 (Just 1) "")  -- resetting the editor
                         

browserRevertAction :: Browser a -> Browser a
browserRevertAction browser = browser &
     action.~Nothing  & 
     inputMode.~False &
     statusLine.~""   &
     input.~"" 

handleIOException :: IOError -> Browser a -> Browser a
handleIOException e browser = 
     case isPermissionError e of
        True  -> browser & statusLine .~ "Permission error"
        _     -> browser & statusLine .~ "error"
        --  False -> browser

getStatusLine :: Browser a -> String
getStatusLine b = b^.statusLine

--validateFilename ::  String -> Bool
--validateFilename b name = isValid name 
--                           then handleBrowserRename b
returnBrowserError ::  String -> Browser a -> EventM a (Browser a)
returnBrowserError e browser = return $
        browser & inputMode.~False & statusLine.~e & action.~Nothing & input.~""

safeInit :: String -> String
safeInit t | length t == 0 = t
           | otherwise     = init t

