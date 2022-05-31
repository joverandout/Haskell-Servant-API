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

type UserAPI2 = "users" :> Get '[JSON] [User]
           :<|> "albert" :> Get '[JSON] User
           :<|> "isaac" :> Get '[JSON] User

startApp :: IO ()
startApp = do
  putStrLn "Starting web server..."
  withApplication (pure app) $ \port -> do
        putStrLn $ printf "Started on http://localhost:%d (CMD Click)" port
        putStrLn "Press enter to quit."
        ch <- getChar
        print ch

app :: Application
app = serve userAPI2 server2

userAPI2 :: Proxy UserAPI2
userAPI2 = Proxy

server :: Server API
server = return users2

joe :: User
joe = User "Joe Moore" "Joe@gmail.com" 21 "club legend"

isaac :: User
isaac = User "Isaac Newton" "isaac@newton.co.uk" 372 "apple guy"

albert :: User
albert = User "Albert Einstein" "ae@mc2.org" 136 "moustache man"

users2 :: [User]
users2 = [isaac, albert, joe]

server2 :: Server UserAPI2
server2 = return users2
     :<|> return albert
     :<|> return isaac
