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
  show = show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Stream x nextStream) = x:(streamToList nextStream)

streamRepeat :: a -> Stream a
streamRepeat x = Stream x (streamRepeat x)

streamMap:: (a -> b) -> Stream a -> Stream b
streamMap f (Stream x nextStream) = Stream (f x) (streamMap f nextStream)
