{-# LANGUAGE OverloadedStrings #-}

module Main (
    main
    ) where

import Data.Aeson
import qualified Data.ByteString.Lazy as B (toStrict)
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import System.Environment

import System.Log.Carma

main :: IO ()
main = getArgs >>= main' where
    main' :: [String] -> IO ()
    main' [l, r] = do
        [lmsgs, rmsgs] <- mapM readLog [l, r]
        mapM_ T.putStrLn $ concat $ zipWith compareGroup lmsgs rmsgs
    main' _ = putStrLn "Usage: carma-log-diff left right"

    compareGroup :: [LogEntry] -> [LogEntry] -> [T.Text]
    compareGroup (lreq : ltail) (rreq : rtail) = concat [
        compareRequests lreq rreq,
        compareTriggers (init ltail) (init rtail),
        compareResponses (last ltail) (last rtail)]

    compareRequests :: LogEntry -> LogEntry -> [T.Text]
    compareRequests (LogRequest luser lurl lmethod ldata) (LogRequest ruser rurl rmethod rdata)
        | (luser, lurl, lmethod) == (ruser, rurl, rmethod) =
            (fromString $ "User: " ++ luser ++ ", URI: " ++ lurl ++ ", method: " ++ lmethod) : diffValues ldata rdata
        | otherwise = error "Requests inequal"
    compareRequests _ _ = error "No request in head of group"

    compareTriggers :: [LogEntry] -> [LogEntry] -> [T.Text]
    compareTriggers ls rs
        | length ls == length rs = concat $ zipWith compareTrigger ls rs
        | otherwise = [fromString (show (length ls) ++ " vs " ++ show (length rs) ++ "triggers")]
        -- TODO: Dump triggers

    compareTrigger :: LogEntry -> LogEntry -> [T.Text]
    compareTrigger (LogTrigger lname ldata) (LogTrigger rname rdata) =
        (if lname == rname then fromString lname else fromString (lname ++ "/" ++ rname)) : diffValues ldata rdata

    compareResponses :: LogEntry -> LogEntry -> [T.Text]
    compareResponses (LogResponse ldata) (LogResponse rdata) = "Response:" : diffValues ldata rdata

    textValue :: Value -> T.Text
    textValue = T.decodeUtf8 . B.toStrict . encode

    diffValues :: Value -> Value -> [T.Text]
    diffValues l r = maybe [] (\(l', r') -> [textValue l', textValue r']) $ compareValues l r

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
