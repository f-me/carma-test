module Main (
    main
    ) where

import Control.Monad.IO.Class
import Control.Concurrent (threadDelay)
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Network.HTTP.Request as R
import System.Log.Carma as L

main :: IO ()
main = do
    cts <- T.readFile "log/db.test.log"
    mapM_ run $ L.parseLog $ T.unpack $ cts
    where
        run :: L.LogEntry -> IO ()
        run l = run' l >> threadDelay 1000000 where
            run' :: L.LogEntry -> IO ()
            run' (L.LogRequest user url method dat) = R.withLogin "http://localhost:8000" user pass $
                printValue $ R.req method url dat
                where
                    pass = case user of
                        "admin" -> ""
                        _ -> user
            run' _ = return ()
        printValue :: MonadIO m => m Value -> m ()
        printValue act = act >>= liftIO . print
