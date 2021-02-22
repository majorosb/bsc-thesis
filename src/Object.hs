{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Object where

import Control.Lens
import Data.Maybe 
import qualified System.FilePath as FP
import qualified System.Directory as Dir
import Control.Exception
import Brick.Types
import Data.Time.Clock

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
                 } deriving(Show)

makeLenses ''Object

getFile :: FilePath -> IO Object     -- Maybe Object would be better?
getFile f = do
        info   <- getInfo f
        isfile <- Dir.doesFileExist f
        path   <- Dir.makeAbsolute f -- catch exceptions aswell
        case isfile of
         True  -> return $ Object f path File info
         False -> return $ Object f path Directory info

getFiles :: FilePath -> IO [Object]
getFiles path = do
        objects <- Dir.listDirectory path
        mapM getFile objects
        


getInfo :: FilePath -> IO Info 
getInfo fileName = do
        size    <- Dir.getFileSize fileName 
        perm    <- Dir.getPermissions fileName
        acctime <- getTimeStampAcc fileName
        modtime <- getTimeStampMod fileName
        return $ Info size perm acctime modtime

getTimeStampAcc :: FilePath -> IO UTCTime
getTimeStampAcc fileName = do
        accTime <- Dir.getAccessTime fileName
        return accTime 

getTimeStampMod :: FilePath -> IO UTCTime
getTimeStampMod fileName = do
        modTime <- Dir.getModificationTime fileName
        return modTime
