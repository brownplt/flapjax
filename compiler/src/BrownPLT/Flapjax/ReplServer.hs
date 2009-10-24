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
import Network.HTTP.Server
import Network.HTTP.Server.Logger (stdLogger)
import Network.URL
import Network.BufferType
import Network.URI
import System.Directory
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.ByteString.Internal (c2w, w2c)
import qualified Data.List as L
import Network.HTTP
import Data.Maybe
import qualified Codec.Binary.Base64.String as Base64

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
flapjaxREPLServer port rootPath = 
  serverWith (Config stdLogger "localhost" 8000)  (handler rootPath)


agentString = "Flapjax REPL Server (www.flapjax-lang.org)"


removeHostHdr :: [Header] -> [Header]
removeHostHdr hdrs = filter f hdrs
  where f hdr = case hdrName hdr of
                  HdrHost -> False
                  otherwise -> True


addBasicAuthHdr :: URI -> [Header] -> [Header]
addBasicAuthHdr uri hdrs = case uriAuthority uri of
  Just (URIAuth userInfo regName port) | length userInfo > 1 ->
    hdrs ++ [Header HdrAuthorization info]
    where withoutTrailingAtSign = L.takeWhile (/='@') userInfo
          base64UserInfo = Base64.encode withoutTrailingAtSign
          info = "Basic " ++ base64UserInfo
          hostname = regName ++ ':':(show port)
  otherwise -> hdrs


redirLocalhost :: Maybe URI -> Maybe URI
redirLocalhost (Just uri) = case uriAuthority uri of
  Just auth -> case uriRegName auth == "localhost" of
    True -> Just $ uri { uriAuthority = Just $ auth { uriPort = ":8001" } }
    False -> Just uri
  Nothing -> Just uri
redirLocalhost Nothing = Nothing


handler :: FilePath -> Handler ByteString
handler rootPath sockAddr url request = do
  let path = uriPath (rqURI request)
  putStrLn $ "Servicing request: " ++ path
  case map tail (L.groupBy (const (/='/')) path) of
    ["compile"] -> compileExprService request
    ["ping"] -> do
      putStrLn "Responding to a ping."
      return (ok $ BS.pack $ map c2w "pong")
    ("redirect":path) -> do
      putStrLn "Redirecting..."
      let auth = case uriAuthority (rqURI request) of
                   Just auth -> uriUserInfo auth
                   Nothing -> ""
      let remoteURI = uriScheme (rqURI request) ++ "//" ++ auth ++
                      concat (L.intersperse "/" path) ++
                      uriQuery (rqURI request)
      putStrLn $ "Parsing " ++ remoteURI
      case redirLocalhost $ parseURI remoteURI of
        Just uri -> do
          let hdrs = addBasicAuthHdr uri
                     $ removeHostHdr
                     $ rqHeaders request
          let fwdRequest =  request
                { rqBody = rqBody request,
                  rqURI = uri,
                  rqHeaders = hdrs }
          result <- simpleHTTP fwdRequest
          case result of
            Right resp -> return resp
            Left err -> do
              putStrLn (show err)
              return four04
        Nothing -> do
          putStrLn "COuld not parse URL"
          return four04
    pathSegments -> do
      putStrLn "Serving local file..."
      let filePath = joinPath pathSegments -- totally, totally insecure
      exists <- doesFileExist filePath
      case exists of
        True -> do   
          bytes <- BS.readFile filePath
          putStrLn $ "Read a file of length " ++ show (BS.length bytes)
          return (ok bytes)
        False -> return four04
