{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedLists       #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}


module OpenID where


import Protolude

import Data.Aeson (FromJSON (..), (.:))
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Types as AeT
import qualified Data.ByteString.Lazy as LBS
import qualified Data.List as List
import qualified Data.Text as Text
import Jose.Jwt (Jwt (..), decodeClaims)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.HTML.Blaze (HTML)
import qualified System.Random as Random
import Text.Blaze (ToMarkup (..))
import qualified Text.Blaze.Html as H
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
import Text.Blaze.Renderer.Utf8 (renderMarkup)

import qualified Web.OIDC.Client as O
import Keys
import qualified Protolude.Conv as Conv



type IdentityRoutes a = 
    "login" :> (
                Get '[JSON] NoContent
                :<|> "cb" :> QueryParam "error" Text
                          :> QueryParam "code" Text
                          :> Get '[HTML] User)

type APIKey = ByteString
type Account = Text.Text
type Conf = [(APIKey,Account)]

data Customer = Customer {
    account   :: Account
  , apiKey    :: APIKey
  , mail      :: Maybe Text
  , fullName  :: Maybe Text
}


redirects :: (Conv.StringConv s ByteString) => s -> Servant.Handler ()
redirects url = throwError err302 { errHeaders = [("Location",toS url)]}

data OIDCEnv = OIDCEnv { 
    oidc            :: O.OIDC                             
  , mgr             :: Manager
  , genState        :: IO ByteString
  , prov            :: O.Provider
  , redirectUri     :: ByteString
  , clientId        :: ByteString
  , clientPassword  :: ByteString
}

data AuthInfo = AuthInfo {
      email         :: Text
    , emailVerified :: Bool
    , name          :: Text
} deriving (Eq, Show, Generic)

data User = User {
    userId            :: Text
  , userSecret        :: Text
  , localStorageKey   :: Text
  , redirectUrl       :: Maybe Text
} deriving (Show, Eq, Ord)


instance FromJSON AuthInfo where
    parseJSON (JSON.Object v) = do
      email       ::  Text <- v .: "email"
      email_verified  :: Bool <- v .: "email_verified"
      name            :: Text <- v .: "name"
      return $ AuthInfo (toS email) email_verified (toS name)
    parseJSON invalid = AeT.typeMismatch "Coord" invalid

    
instance JSON.ToJSON AuthInfo where
  toJSON (AuthInfo e ev n) = 
    JSON.object [ "email"   JSON..= (toS e :: Text)
                , "email_verified"  JSON..= ev
                , "name"  JSON..= (toS n :: Text)]


instance ToMarkup User where
  toMarkup User{..} = H.docTypeHtml $ do
    H.head $
      H.title "Logged In"
    H.body $ do
      H.h1 "Logged in"
      H.p (H.toHtml ("Successful login with id " <> userId))
      H.script (H.toHtml ("localStorage.setItem('" <> localStorageKey <> "', '" <> userSecret <> "');"
                            <> "localStorage.setItem('user-id','" <> userId <> "';"
                            <> "window.location='" <> fromMaybe "/" redirectUrl <> "';"
                            ));


type LoginHandler = AuthInfo -> IO (Either Text User)


initOIDC :: OIDCConf -> IO OIDCEnv
initOIDC OIDCConf{..} = do
    mgr <- newManager tlsManagerSettings
    prov <- O.discover "https://accounts.google.com" mgr
    let oidc = O.setCredentials clientId clientPassword redirectUri (O.newOIDC prov)
    return OIDCEnv {
        oidc = oidc
      , mgr = mgr
      , genState = genRandomBS
      , prov = prov
      , redirectUri = redirectUri
      , clientId = clientId
      , clientPassword = clientPassword
    }


genOIDCURL :: OIDCEnv -> IO ByteString
genOIDCURL OIDCEnv {..} = do
  st <- genState
  let oidcCreds = O.setCredentials clientId clientPassword redirectUri (O.newOIDC prov)
  loc <- O.getAuthenticationRequestUrl oidcCreds [O.openId, O.email, O.profile] (Just st) []
  return (show loc)

handleLogin :: OIDCEnv -> Servant.Handler NoContent
handleLogin oidcenv = do
  loc <- liftIO (genOIDCURL oidcenv)
  redirects loc
  return NoContent



handleLoggedIn :: OIDCEnv -> LoginHandler -> Maybe Text -> Maybe Text -> Servant.Handler User
handleLoggedIn oidcenv handleSuccessfulId err mcode =
  case err of
    Just errorMsg -> forbidden errorMsg
    Nothing -> case mcode of
      Just oauthCode -> do
        tokens <- liftIO $ O.requestTokens (oidc oidcenv) (toS oauthCode) (mgr oidcenv)
        putText . show . O.claims . O.idToken $ tokens
        let jwt = toS . unJwt . O.jwt . O.idToken $ tokens
            eAuthInfo = decodeClaims jwt :: Either O.JwtError (O.JwtHeader,AuthInfo)
        case eAuthInfo of
          Left jwtErr -> forbidden $ "JWT decode/check problem: " <> show jwtErr
          Right (_,authInfo) ->
            if emailVerified authInfo
              then do
                user <- liftIO $ handleSuccessfulId authInfo
                either forbidden return user
              else forbidden "Please verify your email"
      Nothing -> do
        liftIO $ putText "No code param"
        forbidden "no code parameter given"

serveOIDC :: OIDCEnv -> LoginHandler -> Server (IdentityRoutes a)
serveOIDC oidcenv loginHandler = 
  handleLogin oidcenv :<|> handleLoggedIn oidcenv loginHandler


data Homepage = Homepage

instance ToMarkup Homepage where
  toMarkup Homepage = H.docTypeHtml $ do
    H.head $ do
      H.title "OpenID Connect Servant Example"
      H.style (H.toHtml ("body { font-family: monospace; font-size: 18px; }" :: Text.Text))
    H.body $ do
      H.h1 "OpenID Connect Servant Example"
      H.div $
        H.a ! HA.href "/login" $ "Click here to login"
      H.ul $ do
        H.li $ do
          H.span "API Key in Local storage: "
          H.script (H.toHtml ("document.write(localStorage.getItem('api-key'));" :: Text.Text))
        H.li $ do
          H.span "User ID in Local storage: "
          H.script (H.toHtml ("document.write(localStorage.getItem('user-id'));" :: Text.Text))


genRandomBS :: IO ByteString
genRandomBS = do
  g <- Random.newStdGen
  Random.randomRs (0, n) g & take 42 & fmap toChar & readable 0 & toS & return
  where
    n = length letters - 1
    toChar i = letters List.!! i
    letters = ['A'..'Z'] <> ['0'..'9'] <> ['a'..'z']
    readable :: Int -> [Char] -> [Char]
    readable _ [] = []
    readable i str =
      let blocksize = case n of
            0 -> 8
            1 -> 4
            2 -> 4
            3 -> 4
            _ -> 12
          block = take blocksize str
          rest = drop blocksize str
      in if List.null rest
         then str
         else block <> "-" <> readable (i+1) rest

customerFromAuthInfo :: AuthInfo -> IO Customer
customerFromAuthInfo authinfo = do
  apikey <- genRandomBS
  return Customer { account = toS (email authinfo)
                  , apiKey = apikey
                  , mail = Just (toS (email authinfo))
                  , fullName = Just (toS (name authinfo))
                  }

handleOIDCLogin :: LoginHandler
handleOIDCLogin authInfo = do
  custInfo <- customerFromAuthInfo authInfo
  if emailVerified authInfo
    then return . Right . customerToUser $ custInfo
    else return (Left "You emails is not verified by your provider. Please verify your email.")
  where
    customerToUser :: Customer -> User
    customerToUser c =
      User { userId = toS (account c)
           , userSecret = toS (apiKey c)
           , redirectUrl = Nothing
           , localStorageKey = "api-key"
           }


data Err = Err { errTitle :: Text
               , errMsg :: Text }

instance ToMarkup Err where
  toMarkup Err{..} = H.docTypeHtml $ do
    H.head $ do
      H.title "Error"
    H.body $ do
      H.h1 (H.a ! HA.href "/" $ "Home")
      H.h2 (H.toHtml errTitle)
      H.p (H.toHtml errMsg)

format :: ToMarkup a => a -> LBS.ByteString
format err = toMarkup err & renderMarkup

appToErr :: ServerError -> Text -> ServerError
appToErr x msg = x
  { errBody = toS $ format (Err (toS (errReasonPhrase x)) msg)
  , errHeaders =  [("Content-Type","text/html")]}

unauthorized :: (MonadError ServerError m) => Text -> m a
unauthorized = throwError . unauthorizedErr

unauthorizedErr :: Text -> ServerError
unauthorizedErr = appToErr err401

forbidden :: (MonadError ServerError m) => Text -> m a
forbidden = throwError . forbiddenErr

forbiddenErr :: Text -> ServerError
forbiddenErr = appToErr err403

notFound :: ( MonadError ServerError m) => Text -> m a
notFound = throwError . notFoundErr

notFoundErr :: Text -> ServerError
notFoundErr = appToErr err404

preconditionFailed :: ( MonadError ServerError m) => Text -> m a
preconditionFailed = throwError . preconditionFailedErr

preconditionFailedErr :: Text -> ServerError
preconditionFailedErr = appToErr err412

serverError :: ( MonadError ServerError m) => Text -> m a
serverError = throwError . serverErrorErr

serverErrorErr :: Text -> ServerError
serverErrorErr = appToErr err500