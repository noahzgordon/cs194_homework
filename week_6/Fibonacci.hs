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
  show = show . take 50 . streamToList

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
nats = streamFromSeed (+1) 0

evens :: Stream Integer
evens = streamFromSeed (+2) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream a substreamA) streamB 
  = Stream a (interleaveStreams streamB substreamA)

ruler :: Stream Integer
ruler = interleaveStreams zeros pof2s
  where zeros = (streamRepeat 0)
        pof2s = undefined

