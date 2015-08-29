{-# OPTIONS_GHC -Wall #-}

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (+1) $ map (*2) $ filter (flip notElem noList) [1..n]
  where noList    = map (\x -> fst x + snd x + (2 * fst x * snd x)) cartProds
        cartProds = cartProd [1..n] [1..n]

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]
