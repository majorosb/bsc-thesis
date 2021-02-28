{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
module Object where

import Control.Lens
import qualified System.Directory as Dir
import qualified Brick.AttrMap as A
import Brick.Widgets.List (listAttr)
import Data.Time.Clock
import Data.Time.Calendar.OrdinalDate
import Control.Exception

data Object = Object { _name :: String,
                       _path :: FilePath,
                       _filetype :: FileType,
                       _info :: Info 
                     } 

data FileType = File | SymbolicLink | Directory
                    -- ^ POSIX: either file or directory link; Windows: file link
 deriving (Bounded, Enum, Eq, Ord, Read, Show)

instance Show Object where
        show Object{_name = name, _filetype = t, _path = path} =
                case t of 
                 File         -> "- " ++ name 
                 Directory    -> "+ " ++ name
                 SymbolicLink -> "- " ++ name

data Info = Info { _size       :: Integer,
                   _permission :: Dir.Permissions,
                   _acctime    :: UTCTime,
                   _modtime    :: UTCTime
                 } | Fail deriving(Show)

makeLenses ''Object


getFile :: FilePath -> IO Object     -- Maybe Object would be better?
getFile f = do
        fileInfo   <- getInfo f
        isfile     <- Dir.doesFileExist f
        filePath   <- Dir.makeAbsolute f -- catch exceptions aswell
        case isfile of
         True  -> return $ Object f filePath File fileInfo
         False -> return $ Object f filePath Directory fileInfo

getFiles :: FilePath -> IO [Object]
getFiles filePath = do
        objects <- Dir.listDirectory filePath
        mapM getFile objects
        


getInfo :: FilePath -> IO Info 
getInfo fileName = handle errorHandler $ do
        size    <- Dir.getFileSize fileName 
        perm    <- Dir.getPermissions fileName
        acctime <- getTimeStampAcc fileName
        modtime <- getTimeStampMod fileName
        return $ Info size perm acctime modtime
        where 
                errorHandler :: SomeException -> IO Info
                errorHandler _ = return Fail

getTimeStampAcc :: FilePath -> IO UTCTime
getTimeStampAcc fileName = do
        accTime <- Dir.getAccessTime fileName
        return accTime 

getTimeStampMod :: FilePath -> IO UTCTime
getTimeStampMod fileName = do
        modTime <- Dir.getModificationTime fileName
        return modTime

baseAttr :: A.AttrName
baseAttr = listAttr <> A.attrName "window"

fileTypeToAttr :: FileType -> A.AttrName
fileTypeToAttr File      =  baseAttr <> A.attrName "file"
fileTypeToAttr Directory =  baseAttr <> A.attrName "dir"
