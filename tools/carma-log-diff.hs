{-# LANGUAGE OverloadedStrings #-}

module Main (
    main
    ) where

import Control.Arrow
import Control.Monad
import Control.Monad.Writer
import Control.Monad.State
import Data.Aeson
import qualified Data.ByteString.Char8 as C8
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

readFileUtf8 :: FilePath -> IO T.Text
readFileUtf8 f = fmap T.decodeUtf8 $ C8.readFile f

main :: IO ()
main = getArgs >>= main' where
    main' :: [String] -> IO ()
    main' [l, r] = do
        hSetEncoding stdout utf8
        main'' l r T.putStrLn
    main' [l, r, o] = withFile o WriteMode $ \h -> do
        hSetEncoding h utf8
        main'' l r (T.hPutStrLn h)
    main' _ = putStrLn "Usage: carma-log-diff left right"

    main'' :: String -> String -> (T.Text -> IO ()) -> IO ()
    main'' l r outText = do
        tbls <- loadTables "resources/site-config/models" "resources/site-config/field-groups.json"
        let
            -- | Keys to remove from comparison
            dontCompare :: [T.Text]
            dontCompare = ["temperature"] ++ timeColumns

            timeColumns = map (fromString . columnName) $
                filter ((== "timestamp") . columnType) $ concatMap tableFields tbls

            -- | Remove keys
            removeKeys :: [T.Text] -> Value -> Value
            removeKeys ks (Object v) = Object $ HM.map (removeKeys ks) $ HM.filterWithKey notKey v where
                notKey :: T.Text -> Value -> Bool
                notKey k _ = k `notElem` ks
            removeKeys ks v = v

            diffValues :: Value -> Value -> Diff
            diffValues l r = maybe (return ()) (\(l', r') -> tell (map textValue [l', r']) >> markDiff) $
                (compareValues `on` (removeKeys dontCompare)) l r

            runCompareGroup :: Int -> [(Int, LogEntry)] -> [(Int, LogEntry)] -> [T.Text]
            runCompareGroup i ls rs = if isDiff then output else [] where
                ((_, output), isDiff) = runState (runWriterT $ compareGroup i ls rs) False

            compareGroup :: Int -> [(Int, LogEntry)] -> [(Int, LogEntry)] -> Diff
            compareGroup i ls rs = do
                tell [fromString $ "========\t" ++ show i ++ "  ========"]
                tell [fromString $ "at lines " ++ show lline ++ "/" ++ show rline]
                compareGroup' (map snd ls) (map snd rs)
                where
                    lline = fst $ head ls
                    rline = fst $ head rs
                    compareGroup' (lreq : ltail) (rreq : rtail) = do
                        compareRequests lreq rreq
                        compareTriggers (init ltail) (init rtail)
                        compareResponses (last ltail) (last rtail)

            compareRequests :: LogEntry -> LogEntry -> Diff
            compareRequests (LogRequest luser lurl lmethod ldata) (LogRequest ruser rurl rmethod rdata)
                | (lurl, lmethod) == (rurl, rmethod) = do
                    tell [fromString $ "User: " ++ fromMaybe "<null>" luser ++ ", URI: " ++ lurl ++ ", method: " ++ lmethod]
                    diffValues ldata rdata
                | otherwise = do
                    tell [fromString $ "Requests inequal:"]
                    tell [fromString $ "User: " ++ fromMaybe "<null>" luser ++ ", URI: " ++ lurl ++ ", method: " ++ lmethod]
                    tell [fromString $ "User: " ++ fromMaybe "<null>" ruser ++ ", URI: " ++ rurl ++ ", method: " ++ rmethod]
                    markDiff
                | otherwise = error "Requests inequal"
            compareRequests _ _ = error "No request in head of group"

            compareTriggers :: [LogEntry] -> [LogEntry] -> Diff
            compareTriggers ls rs
                | length ls == length rs = zipWithM_ compareTrigger ls rs
                | otherwise = do
                    tell [fromString (show (length ls) ++ " vs " ++ show (length rs) ++ " triggers")]
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

        mapM_ outText $ concat $ zipWith3 runCompareGroup [1..] lmsgs rmsgs

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
                HM.intersectionWith (\ x y -> if x == y then Nothing else compareValues x y) l r
    compareValues x y = Just (x, y)

    readLog :: String -> IO [[(Int, LogEntry)]]
    readLog = fmap (map (map (second logEntry)) . groupLinedRequests . parseLog . T.unpack) . readFileUtf8
