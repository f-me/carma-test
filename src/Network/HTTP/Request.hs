{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

-- | Module provides function to send GET, POST and PUT requests
module Network.HTTP.Request (
    login,
    get,
    req,
    put,
    post,
    withLogin, withoutLogin
    ) where

import Control.Exception (ErrorCall(..))
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson
import Data.ByteString.Lazy (ByteString, toStrict)
import Data.Conduit
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

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
get :: (MonadResource m, MonadBaseControl IO m, FromJSON r) => String -> ReaderT String (GenericBrowserAction m) (Either String r)
get url = do
    baseUrl <- ask
    lift $ do
        r <- liftIO $ parseUrl $ baseUrl ++ url
        makeRequestLbs r >>= decodeJSON

-- | PUT or POST
req :: (MonadResource m, MonadBaseControl IO m, ToJSON v, FromJSON r) => String -> String ->  v -> ReaderT String (GenericBrowserAction m) (Either String r)
req reqMethod url dat = do
    baseUrl <- ask
    lift $ do
        r <- liftIO $ do
            req' <- parseUrl $ baseUrl ++ url
            return $ req' {
                method = fromString reqMethod,
                requestBody = RequestBodyLBS $ encode dat,
                requestHeaders = (hContentType, "application/json") : requestHeaders req',
                responseTimeout = Nothing,
                checkStatus = \_ _ -> Nothing }
        makeRequestLbs r >>= decodeJSON

-- | Put JSON data on relative URL
put :: (MonadResource m, MonadBaseControl IO m, ToJSON v, FromJSON r) => String ->  v -> ReaderT String (GenericBrowserAction m) (Either String r)
put = req "PUT"

-- | Post data at relative URL
post :: (MonadResource m, MonadBaseControl IO m, ToJSON v, FromJSON r) => String -> v -> ReaderT String (GenericBrowserAction m) (Either String r)
post = req "POST"

withLogin :: Manager -> String -> String -> String -> ReaderT String BrowserAction a -> ResourceT IO a
withLogin man url name pass act = do
    browse man $ flip runReaderT url $ do
        login name pass
        act

withoutLogin :: Manager -> String -> ReaderT String BrowserAction a -> ResourceT IO a
withoutLogin man url act = browse man $ runReaderT act url

decodeJSON :: (MonadResource m, MonadBaseControl IO m, FromJSON r) => Response ByteString -> GenericBrowserAction m (Either String r)
decodeJSON response = case checkStatus def (responseStatus response) (responseHeaders response) of
    Nothing -> maybe (return $ Left decodeError) (return . Right) $ decode body
    Just err -> return $ Left $ "Response failed with: " ++ show err
    where
        body = responseBody response
        decodeError = "Can't decode response: " ++ T.unpack (T.decodeUtf8 $ toStrict body)

--decodeJSON = maybe (monadThrow $ ErrorCall "Can't decode JSON") return . decode . responseBody
