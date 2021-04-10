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

data Object = Object { _name       :: String,
                       _path       :: FilePath,
                       _filetype   :: FileType,
                       _isSelected :: Bool,
                       _info       :: Maybe Info 
                     } 
instance Eq Object where 
        Object{_filetype = f1} == Object{_filetype = f2} = f1 == f2

instance Ord Object where
        compare Object{_filetype = f1} Object{ _filetype = f2} = compare f1 f2

instance Show Object where
        show Object{_name = name, _filetype = t, _path = path} =
                case t of 
                 File         -> "- " ++ name 
                 Directory    -> "+ " ++ name
                 SymbolicLink -> "- " ++ name

data Info = Info     { _size       :: Integer,
                       _permission :: Dir.Permissions,
                       _acctime    :: UTCTime,
                       _modtime    :: UTCTime
                     }
data FileType =  SymbolicLink | Directory | File 
                    -- ^ POSIX: either file or directory link; Windows: file link
 deriving (Bounded, Enum, Eq, Ord, Read, Show)

makeLenses ''Object



permissionS :: Dir.Permissions -> String
permissionS p = [ 
                if Dir.readable p   then 'r' else '-',
                if Dir.writable p   then 'w' else '-',
                if Dir.executable p then 'x' else '-',
                if Dir.searchable p then 's' else '-'
                ]

instance Show Info where
        show Info {_size = s, _permission = p} = 
                   show s ++ " " ++ permissionS p

getFile :: FilePath -> IO Object     -- Maybe Object would be better?
getFile f = do
        fileInfo   <- getInfo f
        isfile     <- Dir.doesFileExist f
        filePath   <- Dir.makeAbsolute f -- catch exceptions aswell
        case isfile of
         True  -> return $ Object f filePath File False fileInfo
         False -> return $ Object f filePath Directory False fileInfo

getFiles :: FilePath -> IO [Object]
getFiles filePath = do
        objects <- Dir.listDirectory filePath
        mapM getFile objects

getInfo :: FilePath -> IO (Maybe Info)
getInfo fileName = handle errorHandler $ do
        size    <- Dir.getFileSize fileName 
        perm    <- Dir.getPermissions fileName
        acctime <- getTimeStampAcc fileName
        modtime <- getTimeStampMod fileName
        return $ Just $ Info size perm acctime modtime
        where 
                errorHandler :: SomeException -> IO (Maybe Info)
                errorHandler _ = return Nothing

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

attrFile :: A.AttrName
attrFile = A.attrName "file"

attrDir :: A.AttrName
attrDir = A.attrName "dir"

tEmpty :: A.AttrName
tEmpty = A.attrName "tempty"

tFocused :: A.AttrName
tFocused = A.attrName "tfocused"
