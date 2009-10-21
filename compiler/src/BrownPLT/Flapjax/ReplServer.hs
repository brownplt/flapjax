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
import System.Directory
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.ByteString.Internal (c2w, w2c)
import qualified Data.List as L

parseExpr = do
  whiteSpace
  e <- parseExpression
  eof
  return e


ok :: ByteString -> Response ByteString
ok str = 
  Response (2,0,0) "ok" 
           [ Header HdrCacheControl "no-store, must-revalidate"
           , Header HdrContentLength (show $ BS.length str)
           ]
           str

four04 = Response (4,0,4) "file not found"
           [ Header HdrCacheControl "no-store, must-revalidate"
           , Header HdrContentLength "0"
           ]
           BS.empty


compileExprService req = case (map w2c $ BS.unpack $ rqBody req) of
  'e':'x':'p':'r':'=':escSrc -> do -- total disaster
    let src = urlDecode escSrc
    putStrLn $ "Compiling: "  ++ src
    case parse parseExpr "web request" src of
      Left _ -> return $ ok (BS.pack $ map c2w "throw \'parse error\'")
      Right fxExpr -> do
        jsExpr <- compileExpr defaults fxExpr
        return $ ok (BS.pack (map c2w $ renderExpression jsExpr))
  _ -> do
    putStrLn "compile: no expression provided"
    return $ ok (BS.pack (map c2w "throw \'expression not provided\'"))


flapjaxREPLServer :: Int -> FilePath -> IO ()
flapjaxREPLServer port rootPath = server (handler rootPath)

handler :: FilePath -> Handler ByteString
handler rootPath sockAddr url request = do
  let path = uriPath (rqURI request)
  putStrLn path
  case map tail (L.groupBy (const (/='/')) path) of
    ["compile"] -> compileExprService request
    ["ping"] -> do
      putStrLn "Responding to a ping."
      return (ok $ BS.pack $ map c2w "pong")
    ("redirect":path) -> do
      let remoteURL = "http://" ++  concat (L.intersperse "/" path)
      putStrLn remoteURL
      case parseURI remoteURL of
        Just uri -> do
          let req = mkRequest GET uri
          r <- simpleHTTP req
          case r of
            Right rsp -> return rsp
            Left err -> return four04 -- not really
        Nothing -> return four04 -- not really
    pathSegments -> do
      let filePath = joinPath pathSegments -- totally, totally insecure
      exists <- doesFileExist filePath
      case exists of
        True -> do   
          bytes <- BS.readFile filePath
          putStrLn $ "Read a file of length " ++ show (BS.length bytes)
          return (ok bytes)
        False -> return four04
