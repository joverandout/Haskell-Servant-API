{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Lib
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Data.Maybe

import GHC.Generics (Generic)
import GHC.Conc
import Control.Monad.IO.Class

import DataTypes

main :: IO ()
main = do
    counter <- newCounter
    hspec (spec $ Just counter)

spec :: Maybe (TVar Counter) -> Spec
spec counter = do
    with (return $ app counter) $ do
        describe "GET /testUsers" $ do
            it "responds with 200" $ do
                get "/testUsers" `shouldRespondWith` 200
        describe "GET /testUsers" $ do
            it "responds with 200" $ do
                get "/testUsers" `shouldRespondWith` 200
            it "responds with [User]" $ do
                get "/testUsers" `shouldRespondWith` "[{\"userName\":\"Isaac Newton\",\"userEmail\":\"isaac@newton.co.uk\",\"userAge\":372,\"userOccupation\":\"apple guy\"},{\"userName\":\"Albert Einstein\",\"userEmail\":\"ae@mc2.org\",\"userAge\":136,\"userOccupation\":\"moustache man\"},{\"userName\":\"Joe Moore\",\"userEmail\":\"Joe@gmail.com\",\"userAge\":21,\"userOccupation\":\"club legend\"}]"
        describe "\nCounter unit tests" $ do
            it "'get /counter' responds with a 200" $ do
                get "/counter" `shouldRespondWith` 200
            it "'get /counter' responds with a counter value" $ do
                get "/counter" `shouldRespondWith` "{\"count\":0}"
            it "'increaseCounter' increases the counter by 1" $ do
                increaseCounter (fromJust counter)
                get "/counter" `shouldRespondWith` "{\"count\":1}"
            it "'increaseCounter' can be applied multiple times" $ do
                increaseCounter (fromJust counter)
                increaseCounter (fromJust counter)
                get "/counter" `shouldRespondWith` "{\"count\":3}"
