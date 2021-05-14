{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
module Object where

import Control.Lens
import qualified System.Directory as Dir
import qualified Brick.AttrMap as A
import Brick.Widgets.List (listAttr)
import Data.Time.Clock
import Data.Time.Format
import Control.Exception

data Object =
   Object { _name       :: String,
            _path       :: FilePath,
            _filetype   :: FileType,
            _isSelected :: Bool,
            _info       :: Maybe Info 
          } 
 deriving Eq

instance Ord Object where
        compare Object{_filetype = f1} Object{ _filetype = f2} = compare f1 f2

instance Show Object where
        show Object{_name = name, _filetype = t, _path = path} =
                case t of 
                 File         -> name ++ " " ++ path ++ " " ++show t
                 Directory    -> name ++ " " ++ path ++ " " ++show t
                 SymbolicLink -> name ++ " " ++ path ++ " " ++show t

data Info = Info     { _size       :: Integer,
                       _permission :: Dir.Permissions,
                       _acctime    :: UTCTime,
                       _modtime    :: UTCTime
                     }
 deriving (Eq, Ord)

data FileType =  SymbolicLink | Directory | File 
                    -- ^ POSIX: either file or directory link; Windows: file link
 deriving (Bounded, Enum, Eq, Ord, Read, Show)

makeLenses ''Info
makeLenses ''Object

defTime :: UTCTime -> String
defTime = formatTime defaultTimeLocale "%b %u %R"

defTimeAfterYear :: UTCTime -> String
defTimeAfterYear = formatTime defaultTimeLocale "%y %b %u"

permissionS :: Dir.Permissions -> String
permissionS p = [ 
                if Dir.readable p   then 'r' else '-',
                if Dir.writable p   then 'w' else '-',
                if Dir.executable p then 'x' else '-',
                if Dir.searchable p then 's' else '-'
                ]
convUnit :: Integer -> (String,Float) -> (String,Integer)
convUnit b (f,s) = (f,round $ n/s)
        where n = fromIntegral b :: Float
convertBytes :: Integer -> String 
convertBytes b = case filter (\(_,s) -> s > 1) [convUnit b x | x <- units] of
                   []     -> show b ++ " " ++ "B"
                   l      -> show n ++ " " ++ sizeName
                        where (sizeName,n) = last l 
        where units = [("KB", 1024 ** 1),("MB",1024 ** 2),("GB",1024 ** 3),("TB",1024 ** 4),("PB",1024 ** 5),("EB",1024 ** 6)]
              
instance Show Info where
        show Info {_modtime = m, _size = s, _permission = p} = 
                (convertBytes s) ++ " " ++ permissionS p

getFile :: FilePath -> IO Object     -- Maybe Object would be better?
getFile f = do
        fileInfo   <- getInfo f
        isfile     <- Dir.doesFileExist f
        filePath   <- Dir.makeAbsolute f 
        case isfile of
         True  -> return $ Object f filePath File False fileInfo
         False -> return $ Object f filePath Directory False fileInfo

getFiles :: FilePath -> IO [Object]
getFiles filePath = do
        objects <- Dir.listDirectory filePath
        mapM getFile objects

getInfo :: FilePath -> IO (Maybe Info)
getInfo fileName = handle errorHandler $ do
        fsize    <- Dir.getFileSize fileName 
        perm     <- Dir.getPermissions fileName
        acctime' <- getTimeStampAcc fileName
        modtime' <- getTimeStampMod fileName
        return $ Just $ Info fsize perm acctime' modtime'
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
fileTypeToAttr Directory =  baseAttr <> A.attrName "dir"
fileTypeToAttr _ =  baseAttr <> A.attrName "file"

attrFile :: A.AttrName
attrFile = A.attrName "file"

attrDir :: A.AttrName
attrDir = A.attrName "dir"

tEmpty :: A.AttrName
tEmpty = A.attrName "tempty"

tFocused :: A.AttrName
tFocused = A.attrName "tfocused"

attrFill :: A.AttrName
attrFill = A.attrName "fill"

attrSelected :: A.AttrName
attrSelected = A.attrName "select"

attrStatus :: A.AttrName
attrStatus = A.attrName "status"
