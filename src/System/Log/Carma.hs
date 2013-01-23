{-# LANGUAGE OverloadedStrings #-}

-- | Parsing carma logs
module System.Log.Carma (
    LogEntry(..),
    parseLog,
    parseEntry
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

-- | Log entry
data LogEntry =
    LogRequest String String String Value |
    -- ^ User, URL, method and data
    LogResponse Value |
    -- ^ Response value
    LogTrigger String Value
    -- ^ Trigger entry
        deriving (Eq, Show)

parseLog :: String -> [LogEntry]
parseLog = mapMaybe parseEntry . lines

parseEntry :: String -> Maybe LogEntry
parseEntry line = do
    (name, val) <- parseValue line
    p <- fmap snd $ find (($ name) . fst) parsers
    p (name, val)
    where
        parsers :: [(String -> Bool, (String, Value) -> Maybe LogEntry)]
        parsers = [
            ((== "reqlogger"), parseMaybe req . snd),
            ((== "resplogger"), Just . LogResponse . snd),
            (isPrefixOf triggerPrefix, trig)]
            where
                req :: Value -> Parser LogEntry
                req (Object v) = LogRequest <$>
                    (v .: "user") <*>
                    (v .: "uri") <*>
                    (v .: "method") <*>
                    (v .: "body")
                req _ = empty
                trig :: (String, Value) -> Maybe LogEntry
                trig (nm, v) = Just $ LogTrigger (fromJust $ stripPrefix triggerPrefix nm) v
        triggerPrefix :: String
        triggerPrefix = "update/trigger/"

parseValue :: String -> Maybe (String, Value)
parseValue line = extract $ line =~ logRegex where
    extract:: [[String]] -> Maybe (String, Value)
    extract [[_, name, msg]] = do
        val <- decode . encodeUtf8 . pack $ msg
        return (name, val)
    extract _ = Nothing

logRegex :: String
logRegex = "^[0-9]+/[0-9]+/[0-9]+ [0-9]+:[0-9]+:[0-9]+ \\+[0-9]+\tTRACE\t([a-z/]+)> (.*)$"
