{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Aeson
import Data.Aeson.TH
import Data.Maybe
import DataTypes
import Database

import Network.Wai
import Network.Wai.Handler.Warp ( withApplication )

import Servant
import Servant.JS

import Text.Printf

import Database.SQLite.Simple

import GHC.Generics (Generic)
import GHC.Conc
import Control.Monad.IO.Class

import System.FilePath

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
  writeJSForAPI userAPI vanillaJS (worldwideweb </> "api.js")
  counter <- newCounter
  withApplication (pure $ app (Just counter)) $ \port -> do
        putStrLn $ printf "Started on http://localhost:%d (CMD Click)" port
        putStrLn "Press enter to quit."
        ch <- getChar
        print ch

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

createNewUser :: String -> String -> Int -> String -> IO User
createNewUser name email age occupation = do
  let user = User name email age occupation
  addUserToDB user
  pure user

-- static web filepath
worldwideweb :: FilePath
worldwideweb = "static"

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
    :<|> serveDirectoryFileServer worldwideweb
