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

timeOfMessage :: LogMessage -> TimeStamp
timeOfMessage (Unknown _)          = -1
timeOfMessage (LogMessage _ ts _)  = ts

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) mt = mt
insert lm Leaf = Node Leaf lm Leaf
insert lm1 (Node mt1 lm2 mt2)
    | (timeOfMessage lm1) < (timeOfMessage lm2) = insert lm1 mt1
    | otherwise                                 = insert lm1 mt2

build :: [LogMessage] -> MessageTree
build []     = Leaf
build (x:xs) = insert x $ build xs

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf              = []
inOrder (Node mt1 lm mt2) = (inOrder mt1) ++ [lm] ++ (inOrder mt2)
