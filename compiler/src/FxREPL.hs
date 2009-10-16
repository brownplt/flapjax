-- |A web-server that serves Flapjax files by compiling them to pure HTML
-- and JavaScript.
module Main where

import System.Console.GetOpt
import System.Environment hiding (withArgs)
import BrownPLT.Flapjax.ReplServer
import BrownPLT.Flapjax.Interface
import System.Exit
import Control.Monad
import Data.List
import System.IO

data Option
  = Usage
  deriving (Eq,Ord)

options:: [OptDescr Option]
options =
 [ Option ['h'] ["help"] (NoArg Usage) "shows this help message"
 ]

checkUsage (Usage:_) = do
  putStrLn "Flapjax REPL Server (fxrepl)"
  putStrLn (usageInfo "Usage: fxrepl [OPTION ...] path" options)
  exitSuccess
checkUsage _ = return ()
  

main = do
  argv <- getArgs
  let (permutedArgs,files,errors) = getOpt Permute options argv
  unless (null errors) $ do
    mapM_ (hPutStrLn stderr) errors
    exitFailure
  let args = sort permutedArgs
  checkUsage args

  unless (null args) $ do
    hPutStrLn stderr "invalid arguments, use -h for help"
    exitFailure

  case files of
    [rootPath] -> flapjaxREPLServer 8080 rootPath
    [] -> flapjaxREPLServer 8080 "."
    otherwise -> fail "expected a single root path"
