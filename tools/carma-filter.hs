module Main (
    main
    ) where

import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment

import System.Log.Carma

main :: IO ()
main = getArgs >>= main' where
    main' :: [String] -> IO ()
    main' [from, to] = do
        cts <- T.readFile from
        T.writeFile to $ T.unlines $ filter (isJust . parseMessage . T.unpack) $ T.lines cts
    main' _ = putStrLn "Usage: carma-filter from to"
