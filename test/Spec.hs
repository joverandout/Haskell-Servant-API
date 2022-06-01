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

main :: IO ()
main = do
    counter <- newCounter
    hspec (spec $ Just counter)

counterOne :: Counter
counterOne = Counter 1

-- testCounter :: Counter -> TVar Counter
-- testCounter counterOne = TVar counterOne
-- I WANT TO MAKE A NEW TVAR COUNTER BUT CANNOT!

spec :: Maybe (TVar Counter) -> Spec
spec counter = do
    with (return $ app counter) $ do
        describe "GET /testUsers" $ do
            it "responds with 200" $ do
                get "/testUsers" `shouldRespondWith` 200
            it "responds with [User]" $ do
                let users = "[{\"userName\":\"Isaac Newton\",\"userEmail\":\"isaac@newton.co.uk\",\"userAge\":372,\"userOccupation\":\"apple guy\"},{\"userName\":\"Albert Einstein\",\"userEmail\":\"ae@mc2.org\",\"userAge\":136,\"userOccupation\":\"moustache man\"},{\"userName\":\"Joe Moore\",\"userEmail\":\"Joe@gmail.com\",\"userAge\":21,\"userOccupation\":\"club legend\"}]"
                get "/testUsers" `shouldRespondWith` users
        -- describe "Increase Counter" $ do
        --     it "Counter Increases" $ do
        --         increaseCounter (TVar testCounter) `shouldRespondWith` 1
