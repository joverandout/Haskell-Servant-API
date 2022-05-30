{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Text.Printf


data User = User
  { userName :: String
  , userEmail :: String
  , userAge :: Int
  , userOccupation :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type API = "users" :> Get '[JSON] [User]

startApp :: IO ()
startApp = do
  putStrLn "Starting web server..."
  withApplication (pure app) $ \port -> do
        putStrLn $ printf "Started on http://localhost:%d (CMD Click)" port
        putStrLn "Press enter to quit."
        ch <- getChar
        print ch

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return users

users :: [User]
users = [ -- User 1 "Isaac" "Newton"
        -- , User 2 "Albert" "Einstein"
        ]
