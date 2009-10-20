module BrownPLT.Flapjax.ReplServer
  ( flapjaxREPLServer
  ) where

import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.Map as M
import System.FilePath
import Control.Monad.Trans
import Flapjax.Compiler
import BrownPLT.JavaScript.Parser (parseExpression)
import BrownPLT.JavaScript.Lexer
import BrownPLT.JavaScript.PrettyPrint
import Text.ParserCombinators.Parsec
import Network.HTTP
import Network.HTTP.Server
import Network.URL
import Network.URI

parseExpr = do
  whiteSpace
  e <- parseExpression
  eof
  return e


ok :: String -> Response String
ok str = 
  Response (2,0,0) "ok" 
           [ Header HdrCacheControl "no-store, must-revalidate"
           , Header HdrContentLength (show $ length str)
           ]
           str


compileExprService req = case (rqBody req) of
  'e':'x':'p':'r':'=':src -> do -- total disaster
    putStrLn $ "Compiling: "  ++ src
    case parse parseExpr "web request" src of
      Left _ -> return $ ok "throw \'parse error\'"
      Right fxExpr -> do
        jsExpr <- compileExpr defaults fxExpr
        return $ ok (renderExpression jsExpr)
  _ -> do
    putStrLn "compile: no expression provided"
    return $ ok "throw \'expression not provided\'"


flapjaxREPLServer :: Int -> FilePath -> IO ()
flapjaxREPLServer port rootPath = server (handler rootPath)

handler :: FilePath -> Handler String
handler rootPath sockAddr url request = do
  let path = uriPath (rqURI request)
  putStrLn path
  case path of
    "/compile" -> compileExprService request
    "/ping" -> do
      putStrLn "Responding to a ping."
      return (ok "pong")
    otherwise -> do
      
      bytes <- readFile (rootPath ++ path)
      return (ok bytes)