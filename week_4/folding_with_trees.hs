{-# OPTIONS_GHC -Wall #-}

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insertNode Leaf

insertNode :: a -> Tree a -> Tree a
insertNode x Leaf = Node 0 Leaf x Leaf
insertNode x (Node _ Leaf v r) = Node 1 (insertNode x Leaf) v r
insertNode x (Node _ l v Leaf) = Node 1 l v (insertNode x Leaf)
insertNode x (Node h lTree@(Node lHeight _ _ _) y rTree@(Node rHeight _ _ _))
  | lHeight > rHeight = insertLeft
  | otherwise         = insertRight
    where lHeight = heightOf lTree
          rHeight = heightOf rTree
          insertLeft  = Node (h + lHeight) (insertNode x lTree) y rTree
          insertRight = Node (h + rHeight) lTree y (insertNode x rTree)

heightOf :: Tree a -> Int
heightOf Leaf           = undefined
heightOf (Node h _ _ _) = h

