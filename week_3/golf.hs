{-# OPTIONS_GHC -Wall #-}
module Golf where

{- SKIPS -}
skips :: [a] -> [[a]]
skips xs = map (everyNth xs) [1..(length xs)]

everyNth :: [a] -> Int -> [a]
everyNth xs n = case drop (n-1) xs of []     -> []
                                      (y:ys) -> y : everyNth ys n

{- LOCAL MAXIMA -}
localMaxima :: [Integer] -> [Integer]
localMaxima []  = []
localMaxima [_] = []
localMaxima (_:_:[]) = []
localMaxima (x:y:z:zs)
  | y > x && y > z = y : localMaxima (y:z:zs)
  | otherwise      = localMaxima (y:z:zs)


{- HISTOGRAM -}
