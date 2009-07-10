-- |A web-server that serves Flapjax files by compiling them to pure HTML
-- and JavaScript.
module Main where

import System.Console.GetOpt
import System.Environment hiding (withArgs)
import BrownPLT.Flapjax.DevServer
import BrownPLT.Flapjax.Interface
import System.Exit
import Control.Monad
import Data.List
import System.IO

data Option
  = Usage
  | Flapjax String
  deriving (Eq,Ord)

options:: [OptDescr Option]
options =
 [ Option ['h'] ["help"] (NoArg Usage) "shows this help message"
 , Option ['f'] ["flapjax-path"] (ReqArg Flapjax "URL") "url of flapjax.js"
 ]

checkUsage (Usage:_) = do
  putStrLn "Flapjax Development Server (fxdev)"
  putStrLn (usageInfo "Usage: fxdev [OPTION ...] path" options)
  exitSuccess
checkUsage _ = return ()
  
getFlapjaxPath :: [Option] -> IO (String,[Option])
getFlapjaxPath ((Flapjax s):rest) = return (s,rest)
getFlapjaxPath rest = do
  path <- getInstalledFlapjaxPath
  return (path, rest)


main = do
  argv <- getArgs
  let (permutedArgs,files,errors) = getOpt Permute options argv
  unless (null errors) $ do
    mapM_ (hPutStrLn stderr) errors
    exitFailure
  let args = sort permutedArgs
  checkUsage args

  (fxPath,args) <- getFlapjaxPath args

  unless (null args) $ do
    hPutStrLn stderr "invalid arguments, use -h for help"
    exitFailure

  case files of
    [rootPath] -> flapjaxDevServer 8080 fxPath rootPath
    [] -> flapjaxDevServer 8080 fxPath "."
    otherwise -> fail "expected a single root path"
