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
histogram :: [Integer] -> String
histogram xs = (rows $ digitCounts xs) ++ separator ++ "\n" ++ axis
  where separator = "=========="
        axis      = "0123456789"

rows :: [Integer] -> String
rows xs = unlines $ map (row xs) (reverse [0..9])
  where row ys n = foldl (addMarker n) [] ys
        addMarker counter accum n = if n > counter then accum ++ "*" else accum ++ " "

digitCounts :: [Integer] -> [Integer]
digitCounts xs = zipWith count [0..9] (repeat xs)
  where count x = fromIntegral . length . filter (\y -> x == y)
