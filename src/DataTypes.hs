{-# LANGUAGE TemplateHaskell #-}


module DataTypes where
import Data.Aeson.TH
import GHC.Generics
import Data.Aeson


data User = User
  { userName :: String
  , userEmail :: String
  , userAge :: Int
  , userOccupation :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)


newtype Counter = Counter {
  count :: Int
} deriving (Num, Show, Generic)

instance ToJSON Counter