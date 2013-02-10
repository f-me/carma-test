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
    loop h = do
        b <- hIsEOF h
        when (not b) $ do
            line <- fmap T.decodeUtf8 $ C8.hGetLine h
            maybe (return ()) run $ parseMessage $ T.unpack line
            loop h

    run :: LogMessage -> IO ()
    run = run' 0 where
        run' :: Int -> LogMessage -> IO ()
        run' n l = catch (runRequest l) (retry n) where
            retry :: Int -> SomeException -> IO ()
            retry n e
                | n >= 10 = do
                    putStrLn "Maximum retries exceeded"
                    throw e
                | otherwise = do
                    putStrLn $ "Failed request with " ++ show e
                    threadDelay 1000000
                    run' (succ n) l

    runRequest :: LogMessage -> IO ()
    runRequest (LogMessage _ (LogRequest (Just user) url method dat)) =
        R.withLogin "http://localhost:8000" user pass $
            printValue $ R.req method url dat
            where
                pass = case user of
                    "admin" -> ""
                    _ -> user
    runRequest _ = return ()
    printValue :: MonadIO m => m Value -> m ()
    printValue act = act >>= liftIO . print
