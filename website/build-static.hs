#!/usr/bin/env runhaskell

import qualified Data.List as L
import System.IO
import System.Directory
import System.FilePath
import Control.Monad
import System.Environment

getDirectoryContentsWithoutDots :: FilePath -> IO [FilePath]
getDirectoryContentsWithoutDots path = do
  files <- getDirectoryContents path
  let f "." = False
      f ".." = False
      f _ = True
  return $ filter f files

sourceDestPairs :: FilePath -> FilePath -> IO [(FilePath,FilePath)]
sourceDestPairs sourceDir destDir = do
  sourceNames <- getDirectoryContentsWithoutDots sourceDir
  let sources = map (sourceDir </>) sourceNames
  let destinations = map (destDir </>) sourceNames
  sourceSubdirs <- filterM doesDirectoryExist sources
  let destSubdirs = map (destDir</>) (map takeFileName sourceSubdirs)
  pathss <- mapM (uncurry sourceDestPairs) (zip sourceSubdirs destSubdirs)
  return $ (zip sources destinations) ++ (concat pathss)



syncFolders :: FilePath -- ^source
            -> FilePath -- ^destination
            -> (FilePath -> FilePath -> IO ())
            -> IO ()
syncFolders source destination action =  do
  pairs <- sourceDestPairs source destination
  mapM_ (\(src,dest) -> action src dest) pairs

copyAction :: FilePath -> FilePath -> IO ()
copyAction src dest = do
  isDir <- doesDirectoryExist src
  case isDir of
    True -> do 
      isDestDir <- doesDirectoryExist dest
      case isDestDir of
        True -> return ()
        False -> do
          isDestFile <- doesFileExist dest
          case isDestFile of
            True -> removeFile dest
            False -> createDirectory dest
    False -> do -- assume it's a file
      isDestFile <- doesFileExist dest
      case isDestFile of
        False -> do
          putStrLn $ "Copying " ++ src ++ " to " ++ dest ++ " (new file) ..."
          copyFile src dest
        True -> do
          srcTime <- getModificationTime src
          destTime <- getModificationTime dest
          when (srcTime > destTime) $ do
            putStrLn $ "Copying " ++ src ++ " to " ++ dest
            copyFile src dest

includeLine = "<!--#INCLUDE virtual=\"" 

processInclude :: FilePath -> String -> IO String
processInclude srcDir line = case includeLine `L.isPrefixOf` line of
  True -> do
    let includeFile = L.takeWhile (/= '"') (drop (length includeLine) line)
    readFile (srcDir</>includeFile)
  False -> return line

ssiAction :: FilePath -> FilePath -> IO ()
ssiAction src dest = do
  putStrLn $ "Processing " ++ src ++ "..."
  text <- readFile src
  lines <- mapM (processInclude (takeDirectory src)) (L.lines text)
  writeFile (addExtension (dropExtension dest) "html") (unlines lines)

ssiAndCopyAction src dest = case takeExtension src of
  ".shtml" -> ssiAction src dest
  ".xml" -> return ()
  otherwise -> copyAction src dest

main = do
  [src,dest] <- getArgs
  syncFolders src dest ssiAndCopyAction
