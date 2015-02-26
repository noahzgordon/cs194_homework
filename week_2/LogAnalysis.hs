{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log
import Data.Char

isNumeric :: String -> Bool
isNumeric []     = True
isNumeric (x:xs) = isDigit x && isNumeric xs

parseFromWords :: [String] -> LogMessage
parseFromWords z@("I":ts:strings) 
      | isNumeric ts = LogMessage Info (read ts :: Int) (unwords strings)
      | otherwise    = Unknown $ unwords z
parseFromWords z@("W":ts:strings)
      | isNumeric ts = LogMessage Warning (read ts :: Int) (unwords strings)
      | otherwise    = Unknown $ unwords z
parseFromWords z@("E":x:ts:strings)
      | (isNumeric x) && (isNumeric ts) = LogMessage (Error (read x :: Int)) (read ts :: Int) (unwords strings)
      | otherwise                       = Unknown $ unwords z
parseFromWords z = Unknown $ unwords z

parseMessage :: String -> LogMessage
parseMessage x = parseFromWords $ words x

parse :: String -> [LogMessage]
parse = map parseMessage . lines
