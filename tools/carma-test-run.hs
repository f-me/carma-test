module Main (
    main
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent (threadDelay)
import Control.Exception
import Data.Aeson
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.IO

import Network.HTTP.Request as R
import System.Log.Carma

readFileUtf8 :: FilePath -> IO T.Text
readFileUtf8 f = fmap T.decodeUtf8 $ C8.readFile f

main :: IO ()
main = withFile "log/db.test.log" ReadMode loop where
    loop :: Handle -> IO ()
    loop = R.withoutLogin "http://localhost:8000" . loop' where
        loop' h = do
            b <- liftIO $ hIsEOF h
            when (not b) $ do
                line <- liftIO $  fmap T.decodeUtf8 $ C8.hGetLine h
                maybe (return ()) run $ parseMessage $ T.unpack line
                loop' h

        run (LogMessage _ (LogRequest (Just user) url method dat)) =
            printValue $ R.req method url dat
        run _ = return ()

        printValue :: MonadIO m => m Value -> m ()
        printValue act = act >>= liftIO . print
