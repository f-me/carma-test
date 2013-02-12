module Main (
    main
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException)
import Data.Aeson
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as B (toStrict)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network (withSocketsDo)
import Network.HTTP.Conduit (withManager)
import System.IO

import Network.HTTP.Request as R
import System.Log.Carma

readFileUtf8 :: FilePath -> IO T.Text
readFileUtf8 f = fmap T.decodeUtf8 $ C8.readFile f

main :: IO ()
main = withFile "log/db.test.log" ReadMode loop where
    loop :: Handle -> IO ()
    loop h = withSocketsDo $ withManager $ \man -> do
        R.withoutLogin man "http://localhost:8000" $ loop' h
        where
            loop' h = do
                b <- liftIO $ hIsEOF h
                when (not b) $ do
                    line <- liftIO $  fmap T.decodeUtf8 $ C8.hGetLine h
                    maybe (return ()) run $ parseMessage $ T.unpack line
                    loop' h

            run (LogMessage _ (LogRequest (Just user) url method dat)) =
                R.req method url dat >>= either printError printValue
            run _ = return ()

            printError :: MonadIO m => String -> m ()
            printError e = liftIO $ putStrLn $ "Response error: " ++ e

            printValue :: MonadIO m => Value -> m ()
            printValue = liftIO . putStrLn . T.unpack . T.decodeUtf8 . B.toStrict . encode
