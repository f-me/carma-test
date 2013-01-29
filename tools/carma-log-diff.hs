{-# LANGUAGE OverloadedStrings #-}

module Main (
    main
    ) where

import Control.Monad
import Control.Monad.Writer
import Control.Monad.State
import Data.Aeson
import qualified Data.ByteString.Lazy as B (toStrict)
import qualified Data.HashMap.Strict as HM
import Data.Function (on)
import Data.List
import Data.Maybe
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import System.IO
import System.Environment

import Carma.ModelTables

import System.Log.Carma

type Diff = WriterT [T.Text] (State Bool) ()

markDiff :: Diff
markDiff = lift $ put True

main :: IO ()
main = getArgs >>= main' where
    main' :: [String] -> IO ()
    main' [l, r] = do
        tbls <- loadTables "resources/site-config/models" "resources/site-config/field-groups.json"
        let
            timeColumns = map (fromString . columnName) $
                filter ((== "timestamp") . columnType) $ concatMap tableFields tbls
            -- | Remove times from comparing
            removeTimes :: Value -> Value
            removeTimes (Object v) = Object $ HM.filterWithKey notTime v where
                notTime :: T.Text -> Value -> Bool
                notTime k _ = k `notElem` timeColumns

            diffValues :: Value -> Value -> Diff
            diffValues l r = maybe (return ()) (\(l', r') -> tell (map textValue [l', r']) >> markDiff) $
                (compareValues `on` removeTimes) l r

            runCompareGroup :: [LogEntry] -> [LogEntry] -> [T.Text]
            runCompareGroup ls rs = if isDiff then output else [] where
                ((_, output), isDiff) = runState (runWriterT $ compareGroup ls rs) False

            compareGroup :: [LogEntry] -> [LogEntry] -> Diff
            compareGroup (lreq : ltail) (rreq : rtail) = do
                compareRequests lreq rreq
                compareTriggers (init ltail) (init rtail)
                compareResponses (last ltail) (last rtail)

            compareRequests :: LogEntry -> LogEntry -> Diff
            compareRequests (LogRequest luser lurl lmethod ldata) (LogRequest ruser rurl rmethod rdata)
                | (luser, lurl, lmethod) == (ruser, rurl, rmethod) = do
                    tell [fromString $ "User: " ++ luser ++ ", URI: " ++ lurl ++ ", method: " ++ lmethod]
                    diffValues ldata rdata
                | otherwise = error "Requests inequal"
            compareRequests _ _ = error "No request in head of group"

            compareTriggers :: [LogEntry] -> [LogEntry] -> Diff
            compareTriggers ls rs
                | length ls == length rs = zipWithM_ compareTrigger ls rs
                | otherwise = do
                    tell [fromString (show (length ls) ++ " vs " ++ show (length rs) ++ "triggers")]
                    markDiff

            compareTrigger :: LogEntry -> LogEntry -> Diff
            compareTrigger (LogTrigger lname ldata) (LogTrigger rname rdata) = do
                tell [T.intercalate "/" (map fromString $ nub [lname, rname])]
                diffValues ldata rdata

            compareResponses :: LogEntry -> LogEntry -> Diff
            compareResponses (LogResponse ldata) (LogResponse rdata) = do
                tell ["Response:"]
                diffValues ldata rdata

        [lmsgs, rmsgs] <- mapM readLog [l, r]

        hSetEncoding stdout utf8
        mapM_ T.putStrLn $ concat $ zipWith runCompareGroup lmsgs rmsgs
    main' _ = putStrLn "Usage: carma-log-diff left right"

    textValue :: Value -> T.Text
    textValue = T.decodeUtf8 . B.toStrict . encode

    compareValues :: Value -> Value -> Maybe (Value, Value)
    compareValues (Object l) (Object r) =
        if HM.null l' && HM.null r'
            then Nothing
            else Just (Object l', Object r')
        where
            l' = (HM.difference l r) `HM.union` (HM.map fst lr)
            r' = (HM.difference r l) `HM.union` (HM.map snd lr)
            lr = HM.map fromJust $ HM.filter isJust $
                HM.intersectionWith (\ x y -> if x == y then Nothing else Just (x, y)) l r

    readLog :: String -> IO [[LogEntry]]
    readLog = fmap (map (map logEntry) . groupRequests . parseLog . T.unpack) . T.readFile
