{-# OPTIONS_GHC -Wall #-}

xor :: [Bool] -> Bool
xor = foldr xor' False
  where xor' x y = (x || y) && (not (x && y))

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base $ reverse xs
