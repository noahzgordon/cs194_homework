{-# OPTIONS_GHC -Wall #-}
module Fibonacci where

{- Exercise 1 -}

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib x = fib (x - 1) + fib (x - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

{- Exercise 2 -}

fibs2 :: [Integer]
fibs2 = map fib2 [0..] 
  where fib2 n | n == 0    = 0
               | n == 1    = 1
               | otherwise = fibs2 !! (n - 1) + fibs2 !! (n - 2)

{- Exercise 3 -}

data Stream a = Stream a (Stream a)

instance Show a => Show (Stream a) where
  show = show . take 64 . streamToList

streamToList :: Stream a -> [a]
streamToList (Stream x nextStream) = x:(streamToList nextStream)

{- Exercise 4 -}

streamRepeat :: a -> Stream a
streamRepeat x = Stream x (streamRepeat x)

streamMap:: (a -> b) -> Stream a -> Stream b
streamMap f (Stream x nextStream) = Stream (f x) (streamMap f nextStream)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Stream x (streamFromSeed f (f x))

{- Exercise 5 -}

nats :: Stream Integer
nats = streamFromSeed (+1) 1

evens :: Stream Integer
evens = streamFromSeed (+2) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream a substreamA) streamB 
  = Stream a (interleaveStreams streamB substreamA)

ruler :: Stream Integer
ruler = streamMap greatestPOf2ThatDividesN nats

greatestPOf2ThatDividesN :: Integer -> Integer
greatestPOf2ThatDividesN n
  | odd n     = 0
  | otherwise = last $ powersOf2UpToNThatDivideN n

powersOf2UpToNThatDivideN :: Integer -> [Integer]
powersOf2UpToNThatDivideN n = filter (powerEvenlyDivides n) (powersOf2UpToN n)
  where powerEvenlyDivides x y = x `mod` (2 ^ y) == 0

powersOf2UpToN :: Integer -> [Integer]
powersOf2UpToN n = takeWhile (raisedToTwoIsLessThan n) $ [1..]
  where raisedToTwoIsLessThan x y = 2 ^ y <= x

