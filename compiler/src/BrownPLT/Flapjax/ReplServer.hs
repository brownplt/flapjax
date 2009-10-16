module BrownPLT.Flapjax.ReplServer
  ( flapjaxREPLServer
  ) where

import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.Map as M
import System.FilePath
import Control.Monad.Trans
import Network.WebServer
import Network.WebServer.Files
import Network.WebServer.HTTP.Listen
import Flapjax.Compiler
import BrownPLT.JavaScript.Parser (parseExpression)
import BrownPLT.JavaScript.Lexer
import BrownPLT.JavaScript.PrettyPrint
import Text.ParserCombinators.Parsec

parseExpr = do
  whiteSpace
  e <- parseExpression
  eof
  return e


ret f = do
  r <- f
  return (setHeader "Cache-Control" "no-store, must-revalidate" r)

compileExprService = do
  src <- stringInput "expr"
  liftIO $ putStrLn $ "Compiling: "  ++ src
  case parse parseExpr "web request" src of
    Left _ -> return $ ok "throw \'parse error\'"
    Right e -> do
      e <- liftIO $ compileExpr defaults e
      return $ ok $ (renderExpression e)


flapjaxREPLServer :: Int -> FilePath -> IO ()
flapjaxREPLServer port rootPath = do
  putStrLn "Starting the REPL server..."
  runServer port $ anyOf
    [ dirEnd "ping" (return $ ok "Pong")
    , dirEnd "compile" (ret compileExprService)
    , serveFiles ["index.html"] rootPath
    ]
