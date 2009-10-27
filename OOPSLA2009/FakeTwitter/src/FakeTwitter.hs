module Main where

import qualified Data.Map as M
import Control.Monad.Trans
import Network.HTTP.Server
import Network.HTTP.Server.Logger (stdLogger)
import Network.URL
import Network.BufferType
import Network.URI
import qualified Data.List as L
import Network.HTTP
import Data.Maybe
import Database.CouchDB
import Database.CouchDB.JSON
import Text.JSON
import System.CPUTime
import qualified Codec.Binary.Base64.String as Base64
import qualified Network.HTTP.Server.HtmlForm as Forms
import System.Environment


-- ----------------------------------------------------------------------------
-- Database Interface
--

data User = User
  { userId :: Doc
  , userPassword :: String
  } deriving (Show)

data Tweet = Tweet
  { tweetId :: Integer
  , tweetText :: String
  , tweetUserId :: Doc
  } deriving (Show)


instance JSON User where

  readJSON val = do
    obj <- jsonObject val
    id <- jsonField "_id" obj
    password <- jsonField "password" obj
    return (User id password)

  showJSON (User id password) = JSObject $ toJSObject
    [ ("password", showJSON password)
    ]


instance JSON Tweet where

  readJSON val = do
    obj <- jsonObject val
    id <- jsonField "id" obj
    text <- jsonField "text" obj
    userId <- jsonField "user" obj
    return (Tweet id text userId)
  
  showJSON (Tweet id text userId) = JSObject $ toJSObject
    [ ("id", showJSON id)
    , ("user", showJSON userId)
    , ("text", showJSON text)
    ]


unauthorized :: String -> Response String
unauthorized reason = 
  Response (4,0,1) "unauthorized"
           [ Header HdrCacheControl "no-store, must-revalidate"
           , Header HdrContentLength (show (length reason))
           ]
           reason


badRequest :: String -> Response String
badRequest reason = 
  Response (4,0,0) "bad request"
           [ Header HdrCacheControl "no-store, must-revalidate"
           , Header HdrContentLength (show (length reason))
           ]
           reason


auth :: Request String 
    -> (Doc -> CouchMonad (Response String)) 
    -> IO (Response String)
auth req k = runCouchDB' $ case lookupHeader HdrAuthorization (rqHeaders req) of
  Just ('B':'a':'s':'i':'c':' ':authBase64) -> do
    let authClearText = Base64.decode authBase64
    let (id, colonPassword) =  L.span (/=':') authClearText
    maybeUser <- getDoc (db "users") (doc id)
    case maybeUser of
      Just (_, _, User _ password) -> case ':':password == colonPassword of
        True -> k (doc id)
        False -> do
          liftIO $ putStrLn $ "Invalid password supplied for " ++ id
          return $ unauthorized "invalid username/password"
      Nothing -> do
        liftIO $ putStrLn $ "Invalid username: " ++ id
        return $ unauthorized "invalid username/password"
  otherwise -> return (badRequest "\"missing Authorization header\"")


updateStatus :: Request String
             -> Doc
             -> CouchMonad (Response String)
updateStatus req userId = case rqBody req of
  's':'t':'a':'t':'u':'s':'=':statusEncoded -> do
    let status = unEscapeString statusEncoded
    id <- liftIO $ getCPUTime
    let tweet = Tweet id status userId
    (doc, rev) <- newDoc (db "tweets") tweet
    liftIO $ putStrLn $ "Created new tweet with _id " ++ show doc
    return (ok "true") -- inconsequential
  otherwise -> do
    liftIO $ putStrLn $ "Missing form fields" ++ show (rqBody req)
    return (badRequest "\"missing form fields\"")

ok :: String -> Response String
ok str = 
  Response (2,0,0) "ok" 
           [ Header HdrCacheControl "no-store, must-revalidate"
           , Header HdrContentLength (show (length str))
           ]
           str

getTweets :: Maybe String -> CouchMonad JSValue
getTweets sinceId = do
  let args = case sinceId of
               Nothing ->  [("descending", JSBool True)]
               Just id -> [ ("endkey", showJSON sinceId)
                          , ("descending", JSBool True)
                          ] 
  rows <- queryView (db "tweets") (doc "byid") (doc "byid") args
  let f (_, Tweet id text userId) = JSObject $ toJSObject
        [ ("text", showJSON text)
        , ("user", JSObject $ toJSObject [("name", showJSON userId)])
        , ("id", showJSON id)
        ]
  return (JSArray (map f rows))


timeline :: Request String
         -> Doc
         -> CouchMonad (Response String)
timeline req userId = case uriQuery (rqURI req) of
  '?':'s':'i':'n':'c':'e':'_':'i':'d':'=':sinceId ->  do
    json <- getTweets (Just sinceId)
    return $ ok $ encode json
  otherwise -> do
    json <- getTweets Nothing
    return $ ok $ encode json


handler :: Handler String
handler sockAddr url request = do
  let path = uriPath (rqURI request)
  putStrLn $ "Servicing request: " ++ path
  case map tail (L.groupBy (const (/='/')) path) of
    ["ping"] -> do
      putStrLn "Responding to a ping."
      return (ok "pong")
    ["statuses", "friends_timeline.json"] -> do
      putStrLn $ "Timeline..."
      auth request (timeline request)
    ["statuses", "update.json"] -> auth request (updateStatus request)
    otherwise ->
      return (Response (4,0,4) "not found" [ Header HdrContentLength "0" ] "")


initDB = runCouchDB' $ do
  dropDB "users"
  createDB "users"
  dropDB "tweets"
  createDB "tweets"
  newView "tweets" "byid" 
    [ ViewMap "byid" "function(doc) { emit(doc.id, doc) }" ]


main = do
  args <- getArgs
  case args of
    ["init"] -> initDB
    otherwise -> serverWith (Config stdLogger "localhost" 8001) handler
