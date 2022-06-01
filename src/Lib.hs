{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import System.FilePath
import Servant.JS

import Text.Printf
import GHC.Generics (Generic)
import GHC.Conc
import Control.Monad.IO.Class



data User = User
  { userName :: String
  , userEmail :: String
  , userAge :: Int
  , userOccupation :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type UserAPI = "users" :> Get '[JSON] [User]
           :<|> "albert" :> Get '[JSON] User
           :<|> "isaac" :> Get '[JSON] User
           :<|> "testUsers" :> Get '[JSON] [User]
           :<|> "counter" :> Post '[JSON] Counter
           :<|> "counter" :> Get '[JSON] Counter

type UserAPI' = UserAPI
            :<|> Raw

newtype Counter = Counter {
  count :: Int
} deriving (Num, Show, Generic)

instance ToJSON Counter

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

startApp :: IO ()
startApp = do
  putStrLn "Starting web server..."
  writeJSForAPI userAPI vanillaJS (www </> "api.js")
  counter <- newCounter
  withApplication (pure $ app counter) $ \port -> do
        putStrLn $ printf "Started on http://localhost:%d (CMD Click)" port
        putStrLn "Press enter to quit."
        ch <- getChar
        print ch

app :: TVar Counter -> Application
app counter = serve userAPI' $ server' counter

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

users :: [User]
users = [isaac, albert, joe]

-- used for unit tests
testUsers :: [User]
testUsers = [isaac, albert, joe]


www :: FilePath
www = "static"

server :: TVar Counter -> Server UserAPI
server counter = return users
     :<|> return albert
     :<|> return isaac
     :<|> return testUsers
     :<|> increaseCounter counter
     :<|> currentCounter counter

server' :: TVar Counter -> Server UserAPI'
server' counter = server counter
    :<|> serveDirectoryFileServer www


