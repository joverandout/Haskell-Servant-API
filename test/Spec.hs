{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Lib (unitTestApp)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (return unitTestApp) $ do
    describe "GET /testUsers" $ do
        it "responds with 200" $ do
            get "/testUsers" `shouldRespondWith` 200
        it "responds with [User]" $ do
            let users = "[{\"userName\":\"Isaac Newton\",\"userEmail\":\"isaac@newton.co.uk\",\"userAge\":372,\"userOccupation\":\"apple guy\"},{\"userName\":\"Albert Einstein\",\"userEmail\":\"ae@mc2.org\",\"userAge\":136,\"userOccupation\":\"moustache man\"},{\"userName\":\"Joe Moore\",\"userEmail\":\"Joe@gmail.com\",\"userAge\":21,\"userOccupation\":\"club legend\"}]"
            get "/testUsers" `shouldRespondWith` users
