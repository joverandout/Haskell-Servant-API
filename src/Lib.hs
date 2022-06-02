{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Aeson
import Data.Aeson.TH
import Data.Maybe
import qualified Data.Text as T

import Network.Wai
import Network.Wai.Handler.Warp

import Servant
import Servant.JS

import Text.Printf

import Database.SQLite.Simple

import GHC.Generics (Generic)
import GHC.Conc
import Control.Monad.IO.Class

import System.FilePath


data User = User
  { userName :: String
  , userEmail :: String
  , userAge :: Int
  , userOccupation :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

-- setup all the default paths of the api and the type that they
-- are expected to return. The type of request they are is 
-- specified by either Get or Post.
type UserAPI = "users" :> Get '[JSON] [User]
           :<|> "albert" :> Get '[JSON] User
           :<|> "isaac" :> Get '[JSON] User
           :<|> "testUsers" :> Get '[JSON] [User]
           :<|> "counter" :> Post '[JSON] Counter
           :<|> "counter" :> Get '[JSON] Counter

-- old api used for initial testing
type UserAPI' = UserAPI
            :<|> Raw

newtype Counter = Counter {
  count :: Int
} deriving (Num, Show, Generic)

instance ToJSON Counter

-- initiate counter to zero on startup of web server
newCounter :: IO (TVar Counter)
newCounter = newTVarIO 0

increaseCounter :: MonadIO m => TVar Counter -> m Counter
increaseCounter counter = liftIO . atomically $ do
  oldValue <- readTVar counter
  let newValue = oldValue + 1
  writeTVar counter newValue
  return newValue

currentCounter :: MonadIO m => TVar Counter -> m Counter
currentCounter counter = liftIO $ readTVarIO counter

-- Starts the web server on a random port and opens it in
-- the console, click the link to follow it or manually enter it
-- to your browser
startApp :: IO ()
startApp = do
  putStrLn "Starting web server..."
  writeJSForAPI userAPI vanillaJS (www </> "api.js")
  counter <- newCounter
  withApplication (pure $ app (Just counter)) $ \port -> do
        putStrLn $ printf "Started on http://localhost:%d (CMD Click)" port
        putStrLn "Press enter to quit."
        ch <- getChar
        print ch

data TestField = TestField Int T.Text deriving (Show)

data TestField2 = TestField2 T.Text T.Text Int T.Text deriving (Show)


instance FromRow TestField where
  fromRow = TestField <$> field <*> field

instance ToRow TestField where
  toRow (TestField id_ str) = toRow (id_, str)

instance FromRow TestField2 where
  fromRow = TestField2 <$> field <*> field <*> field <*> field


test = do
  conn <- open "test.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS test (id INTEGER PRIMARY KEY, str TEXT)"
  execute conn "INSERT INTO test (str) VALUES (?)" (Only ("test string 2" :: String))
  execute conn "INSERT INTO test (id, str) VALUES (?,?)" (TestField 13 "test string 3")
  rowId <- lastInsertRowId conn
  executeNamed conn "UPDATE test SET str = :str WHERE id = :id" [":str" := ("updated str" :: T.Text), ":id" := rowId]
  r <- query_ conn "SELECT * from test" :: IO [TestField]
  mapM_ print r
  execute conn "DELETE FROM test WHERE id = ?" (Only rowId)
  close conn

test2 = do
    conn <- open "test.db"
    execute conn "INSERT INTO test (id, str) VALUES (?,?)" (TestField 13 "test string 3")
    close conn

testUsersDatabase userr = do
  conn <- open "test.db"
  execute conn "INSERT INTO users VALUES (?,?,?,?)" (userName userr, userEmail userr, userAge userr, userOccupation userr)
  close conn

testGetFromDB = do
  conn <- open "test.db"
  r <- query_ conn "SELECT * from users" :: IO [TestField2]
  mapM_ print r
  close conn
  

app :: Maybe (TVar Counter) -> Application
app counter = serve userAPI' $ server' counter

-- func to return the proxy element of the api data type
userAPI :: Proxy UserAPI
userAPI = Proxy

userAPI' :: Proxy UserAPI'
userAPI' = Proxy

joe :: User
joe = User "Joe Moore" "Joe@gmail.com" 21 "club legend"

isaac :: User
isaac = User "Isaac Newton" "isaac@newton.co.uk" 372 "apple guy"

albert :: User
albert = User "Albert Einstein" "ae@mc2.org" 136 "moustache man"

-- test object with a set of 3 users
users :: [User]
users = [isaac, albert, joe]

-- used for unit tests
testUsers :: [User]
testUsers = [isaac, albert, joe]



-- static web filepath
www :: FilePath
www = "static"

-- returns the paths of the api, with the json objects that they
server :: Maybe (TVar Counter) -> Server UserAPI
server counter = return users
     :<|> return albert
     :<|> return isaac
     :<|> return testUsers
     :<|> increaseCounter (fromJust counter)
     :<|> currentCounter (fromJust counter)

server' :: Maybe (TVar Counter) -> Server UserAPI'
server' counter = server counter
    :<|> serveDirectoryFileServer www


