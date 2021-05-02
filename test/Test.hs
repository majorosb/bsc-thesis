import Test.Tasty
import Test.Tasty.QuickCheck
import Test.HUnit
import Data.Vector
import System.FilePath.Posix
import Browser
import Tabs
import Object
import Window
import Brick.Widgets.List
import System.Directory
import Control.Lens
import System.Exit
import Data.List

testPath :: IO FilePath
testPath = do
        currDir <- getCurrentDirectory
        path    <- makeAbsolute currDir
        let fullpath = path </> "test"
        return fullpath

test1FileInfo :: IO (Maybe Info)
test1FileInfo = do
        path <- testPath 
        getInfo $ path </> "getFile/file"

test1DirInfo :: IO (Maybe Info)
test1DirInfo = do
        path <- testPath 
        getInfo $ path </> "getFile/dir"

test1File :: IO Object
test1File = do
        info <- test1FileInfo
        path <- testPath
        let fullpath = path </> "getFile" </> "file"
        return $ Object "file" fullpath File False info

test1Dir :: IO Object
test1Dir = do
        info <- test1DirInfo
        path <- testPath
        let fullpath = path </> "getFile" </> "dir"
        return $ Object "dir" fullpath Directory False info

test2Files :: IO [Object]
test2Files = do 
        d <- test1Dir
        f <- test1File
        return [d,f]

test1 = TestCase (do
        f  <- test1File
        f' <- withCurrentDirectory "test/getFile" $ getFile "file"
        assertEqual "Objects are not equal" f f')

test1' = TestCase (do
        f  <- test1Dir
        f' <- withCurrentDirectory "test/getFile" $ getFile "dir"
        assertEqual "Objects are not equal" f f')


test2 = TestCase(do 
        path <- testPath
        files <- withCurrentDirectory (path </> "getFile") (getFiles ".")
        files' <- test2Files
        assertEqual "The two list are not equal" files' (sort files))

test3 = TestCase(do
        path <- testPath 
        let fullpath = path </> "getFile"
        w <- testWindow
        assertEqual "Paths are not the same" (w^.currentDir) fullpath)

test4 = TestCase(do
        path <- testPath 
        let fullpath = path </> "getFile"
        files <- test2Files
        w <- testWindow
        let l = toList $ listElements (w^.objects) 
        assertEqual "Paths are not the same" (sort l) files)

        
testWindow :: IO Window
testWindow = do 
        path <- testPath
        w <- withCurrentDirectory (path </> "getFile") $  newWindow (WindowName 1) "."
        return w

testChangeDir = TestCase(do
        path   <- testPath
        w      <- withCurrentDirectory (path </> "changeDir") $  newWindow (WindowName 1) "."
        newDir <- withCurrentDirectory (path </> "changeDir") $  makeAbsolute "Okay"
        w'     <- withCurrentDirectory (path </> "changeDir") $  changeDir newDir w
        assertEqual "Paths differ" (path </> "changeDir" </> "Okay") (w'^.currentDir))
         
testChangeDir' = TestCase(do
        path   <- testPath
        w      <- withCurrentDirectory (path </> "changeDir") $  newWindow (WindowName 1) "."
        newDir <- withCurrentDirectory (path </> "changeDir") $  makeAbsolute "NoGo"
        w'     <- withCurrentDirectory (path </> "changeDir") $  changeDir newDir w
        assertEqual "Paths differ" (path </> "changeDir") (w'^.currentDir))
testException = TestCase(do
        path   <- testPath
        w      <- withCurrentDirectory (path </> "changeDir") $  newWindow (WindowName 1) "."
        newDir <- withCurrentDirectory (path </> "changeDir") $  makeAbsolute "NoGo"
        w'     <- withCurrentDirectory (path </> "changeDir") $  changeDir newDir w
        assertEqual "Paths differ" (Nothing) (w'^.windowException))
--assertEqual expected actual

tests = TestList [ TestLabel "getFile/file" test1
                 , TestLabel "getFile/dir" test1'
                 , TestLabel "getFiles" test2
                 , TestLabel "Window Path" test3
                 , TestLabel "Window List" test4
                 , TestLabel "Change Dir" testChangeDir
                 , TestLabel "Change Dir permission denied" testChangeDir'
                 , TestLabel "Check exception" testException
                 ]

main :: IO ()
main = do
       results <- runTestTT tests
       if(errors results + failures results == 0)
          then exitWith ExitSuccess
          else exitWith $ ExitFailure 1
       return ()

