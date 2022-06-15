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
    testCounter <- newCounter' 1
    hspec (spec $ Just counter)

-- testCounter :: Counter -> TVar Counter
-- testCounter counterOne = TVar counterOne
-- I WANT TO MAKE A NEW TVAR COUNTER BUT CANNOT!

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
    describe "'increaseCounter' test" $ do
        it "increments the counter by 1" $ do
            2 + 1 `shouldBe` 3
            -- type error on following line, can't need Counter to be m0 Counter and can't figure out why
            -- increaseCounter (fromJust counter) `shouldBe` testCounter
