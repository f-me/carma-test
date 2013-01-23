{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

-- | Module provides function to send GET, POST and PUT requests
module Network.HTTP.Request (
    login,
    get,
    req,
    put,
    post,
    withLogin
    ) where

import Control.Exception (ErrorCall(..))
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Conduit
import Data.String (fromString)

import Network (withSocketsDo)
import Network.HTTP.Types
import Network.HTTP.Conduit
import Network.HTTP.Conduit.Browser

-- | Login with login and password
login :: (MonadResource m, MonadBaseControl IO m) => String -> String -> ReaderT String (GenericBrowserAction m) ()
login name pass = do
    url <- ask
    lift $ do
        r <- liftIO $ do
            req' <- parseUrl $ url ++ "/login"
            return $ urlEncodedBody [
                ("login", fromString name),
                ("password", fromString pass),
                ("avayaExt", ""),
                ("avayaPwd", "")] req'
        _ <- makeRequestLbs r
        return ()

-- | Get JSON data on relative URL
get :: (MonadResource m, MonadBaseControl IO m, FromJSON r) => String -> ReaderT String (GenericBrowserAction m) r
get url = do
    baseUrl <- ask
    lift $ do
        r <- liftIO $ parseUrl $ baseUrl ++ url
        makeRequestLbs r >>= decodeJSON

-- | PUT or POST
req :: (MonadResource m, MonadBaseControl IO m, ToJSON v, FromJSON r) => String -> String ->  v -> ReaderT String (GenericBrowserAction m) r
req reqMethod url dat = do
    baseUrl <- ask
    lift $ do
        r <- liftIO $ do
            req' <- parseUrl $ baseUrl ++ url
            return $ req' {
                method = fromString reqMethod,
                requestBody = RequestBodyLBS $ encode dat,
                requestHeaders = (hContentType, "application/json") : requestHeaders req' }
        makeRequestLbs r >>= decodeJSON

-- | Put JSON data on relative URL
put :: (MonadResource m, MonadBaseControl IO m, ToJSON v, FromJSON r) => String ->  v -> ReaderT String (GenericBrowserAction m) r
put = req "PUT"

-- | Post data at relative URL
post :: (MonadResource m, MonadBaseControl IO m, ToJSON v, FromJSON r) => String -> v -> ReaderT String (GenericBrowserAction m)  r
post = req "POST"

withLogin :: String -> String -> String -> ReaderT String BrowserAction a -> IO a
withLogin url name pass act = withSocketsDo $ do
    man <- newManager def
    runResourceT $ browse man $ flip runReaderT url $ do
        login name pass
        act

decodeJSON :: (MonadResource m, MonadBaseControl IO m, FromJSON r) => Response ByteString -> GenericBrowserAction m r
decodeJSON = maybe (monadThrow $ ErrorCall "Can't decode JSON") return . decode . responseBody
