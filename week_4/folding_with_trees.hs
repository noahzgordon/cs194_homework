{-# OPTIONS_GHC -Wall #-}

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insertNode Leaf

insertNode :: a -> Tree a -> Tree a
insertNode x Leaf = Node 0 Leaf x Leaf
insertNode x (Node _ l y r)
  | height l > height r  = Node (height newR + 1) l y newR
  | otherwise            = Node (height newL + 1) newL y r
    where newL = insertNode x l
          newR = insertNode x r

height :: Tree a -> Integer
height Leaf           = -1
height (Node h _ _ _) = h

isBalanced :: Tree a -> Bool
isBalanced Leaf           = True
isBalanced (Node _ l _ r) = subtreeDifference <= 1 && isBalanced l && isBalanced r
  where subtreeDifference = abs (height l - height r)

