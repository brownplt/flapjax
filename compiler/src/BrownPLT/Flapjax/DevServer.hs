module BrownPLT.Flapjax.DevServer 
  ( flapjaxDevServer
  ) where

import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.Map as M
import System.FilePath
import Control.Monad.Trans
import Network.WebServer
import Network.WebServer.Files
import Network.WebServer.HTTP.Listen
import Text.XHtml (showHtml,toHtml,HTML)
import BrownPLT.Html
import Flapjax.Compiler

serveFxFiles :: MonadIO m
             => FilePath -- ^root path
             -> FilePath -- ^path to Flapjax, relative to the server root
             -> ServerT m Response
serveFxFiles localRootPath fxPath = do
  rq <- getRequest
  liftIO $ do
    maybePath <- uriToLocalPath ["index.fx", "index.html"] localRootPath
                                (rqPaths rq)
    case maybePath of
      Nothing 
        | rqPaths rq == ["flapjax.js"] -> do
            file <- getFile fxPath
            case file of
              Nothing -> do
                putStrLn $ "Could not open " ++ fxPath
                fail "permission denied accessing flapjax.js"
              Just (body, size, modifiedTime) -> do
                renderFile "text/javascript" size modifiedTime body rq
        | otherwise -> do
            putStrLn $ "File not found: " ++ (show $ rqPaths rq)
            fail "file not found"
      Just path
        | takeExtension path == ".fx" -> do
            parseResult <- parseHtmlFromFile path
            case parseResult of
              Left err -> return $ ok (showHtml (toHtml err))
              Right (fxHtml, warnings) -> do
                mapM_ (putStrLn.show) warnings
                (msgs, html) <- compilePage 
                                  (defaults { flapjaxPath = "/flapjax.js" })
                                  fxHtml
                mapM_ (putStrLn.show) msgs
                renderFile "text/html" 0 undefined (BC.pack $ renderHtml html) 
                           rq
        | otherwise -> do
            file <- getFile path
            case file of
              Nothing -> fail "permission denied"
              Just (body, size, modifiedTime) -> do
                let mimeType = M.findWithDefault "application/octet-stream"
                                (takeExtension path) mimeTypes
                renderFile mimeType size modifiedTime body rq


flapjaxDevServer :: Int -> FilePath -> FilePath -> IO ()
flapjaxDevServer port fxPath rootPath = 
  runServer port (serveFxFiles rootPath fxPath)
