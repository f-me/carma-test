{-# LANGUAGE OverloadedStrings #-}

-- | Parsing carma logs
module System.Log.Carma (
    LogMessage(..),
    LogEntry(..),
    parseLog,
    parseMessage,
    groupRequests
    ) where

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
    LogRequest String String String Value |
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
        trig = LogTrigger <$> (v .: "name") <*> (v .: "data")
    parseJSON _ = empty

-- | Read messages from log
parseLog :: String -> [LogMessage]
parseLog = mapMaybe parseMessage . lines

-- | Read one message
parseMessage :: String -> Maybe LogMessage
parseMessage line = do
    (name, val) <- parseValue line
    if any ($name) [(== "reqlogger"), (== "resplogger"), isPrefixOf "update/trigger/"]
        then parseMaybe parseJSON val
        else Nothing

-- | Group requests
-- Every request is followed with several trigger messages and ends with response
-- Function returns list of such groups
groupRequests :: [LogMessage] -> [[LogMessage]]
groupRequests = go . dropWhile (not . isRequest . logEntry) where
    go :: [LogMessage] -> [[LogMessage]]
    go ((msg@(LogMessage thId req)) : msgs) = thisGroup : go tailMsgs where
        (beforeresp, (resp : afterresp)) = break myResponse msgs
        (myBefore, otherBefore) = partition ((== thId) . logThreadId) beforeresp

        thisGroup = msg : (myBefore ++ [resp])
        tailMsgs = otherBefore ++ afterresp

        myResponse (LogMessage thId' (LogResponse _)) = thId == thId'
        myResponse _ = False
    go [] = []

    isRequest :: LogEntry -> Bool
    isRequest (LogRequest {}) = True
    isRequest _ = False

parseValue :: String -> Maybe (String, Value)
parseValue line = extract $ line =~ logRegex where
    extract:: [[String]] -> Maybe (String, Value)
    extract [[_, name, msg]] = do
        val <- decode . encodeUtf8 . pack $ msg
        return (name, val)
    extract _ = Nothing

logRegex :: String
logRegex = "^[0-9]+/[0-9]+/[0-9]+ [0-9]+:[0-9]+:[0-9]+ \\+[0-9]+[ \t]+TRACE[ \t]+([a-z/]+)> (.*)$"
