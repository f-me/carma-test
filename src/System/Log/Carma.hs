{-# LANGUAGE OverloadedStrings #-}

-- | Parsing carma logs
module System.Log.Carma (
    LogMessage(..),
    LogEntry(..),
    parseLog,
    parseMessage,
    groupRequestsOn, groupRequests, groupLinedRequests
    ) where

import Control.Arrow
import Control.Applicative
import Data.Aeson
import Data.Aeson.Types (Parser, parseMaybe)
import Data.List
import Data.Maybe
import Data.Text.Lazy (pack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.String (fromString)
import Text.Regex.Posix ((=~))

-- | Log message
data LogMessage = LogMessage {
    logThreadId :: String,
    logEntry :: LogEntry }
        deriving (Eq, Show)

-- | Log entry
data LogEntry =
    LogRequest (Maybe String) String String Value |
    -- ^ User, URL, method and data
    LogResponse Value |
    -- ^ Response value
    LogTrigger String Value
    -- ^ Trigger entry
        deriving (Eq, Show)

instance FromJSON LogMessage where
    parseJSON (Object v) = LogMessage <$>
        (v .: "threadId") <*>
        (parseJSON (Object v))
    parseJSON _ = empty

instance FromJSON LogEntry where
    parseJSON (Object v) = req <|> resp <|> trig where
        req = do
            (Object inner) <- v .: "request"
            LogRequest <$>
                (inner .: "user") <*>
                (inner .: "uri") <*>
                (inner .: "method") <*>
                (inner .: "body")
        resp = LogResponse <$> (v .: "response")
        trig = LogTrigger <$> (v .: "trigger") <*> (v .: "data")
    parseJSON _ = empty

-- | Read messages from log
parseLog :: String -> [(Int, LogMessage)]
parseLog = map (second fromJust) . filter (isJust . snd) . zip [1..] . map parseMessage . lines

-- | Read one message
parseMessage :: String -> Maybe LogMessage
parseMessage line = do
    (name, val) <- parseValue line
    if any ($name) [(== "detail/req"), (== "detail/resp"), isPrefixOf "update/detail/trigger/"]
        then parseMaybe parseJSON val
        else Nothing

-- | Group requests
-- Every request is followed with several trigger messages and ends with response
-- Function returns list of such groups
groupRequestsOn :: (a -> LogMessage) -> [a] -> [[a]]
groupRequestsOn f = go . dropWhile (not . isRequest . logEntry . f) where
    go (x : xs) = if null respAndRest then [] else thisGroup : go tailMsgs where
        (msg@(LogMessage thId req)) = f x
        (beforeresp, (respAndRest@(~(resp : afterresp)))) = break (myResponse . f) xs
        (myBefore, otherBefore) = partition ((== thId) . logThreadId . f) beforeresp

        thisGroup = x : (myBefore ++ [resp])
        tailMsgs = otherBefore ++ afterresp

        myResponse (LogMessage thId' (LogResponse _)) = thId == thId'
        myResponse _ = False
    go [] = []

    isRequest :: LogEntry -> Bool
    isRequest (LogRequest {}) = True
    isRequest _ = False

groupRequests :: [LogMessage] -> [[LogMessage]]
groupRequests = groupRequestsOn id

groupLinedRequests :: [(Int, LogMessage)] -> [[(Int, LogMessage)]]
groupLinedRequests = groupRequestsOn snd

parseValue :: String -> Maybe (String, Value)
parseValue line = extract $ line =~ logRegex where
    extract:: [[String]] -> Maybe (String, Value)
    extract [[_, name, msg]] = do
        val <- decode . encodeUtf8 . pack $ msg
        return (name, val)
    extract _ = Nothing

logRegex :: String
logRegex = "^[0-9]+/[0-9]+/[0-9]+ [0-9]+:[0-9]+:[0-9]+ \\+[0-9]+[ \t]+TRACE[ \t]+([a-z/]+)> (.*)$"
