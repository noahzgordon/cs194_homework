module JoinList where
import Data.Monoid
import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

{- Exercise 1 -}

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) list1 list2 = Append ((tag list1) `mappend` (tag list2)) list1 list2

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

{- Exercise 2 -}

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty        = Nothing
indexJ 0 (Single _ a) = Just a
indexJ _ (Single _ _) = Nothing
indexJ n (Append _ l r) 
  | n < lSize = indexJ n l
  | otherwise = indexJ (n - lSize) r
  where lSize = getSize . size $ tag l

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty        = Empty
dropJ 0 x            = x
dropJ _ (Single _ _) = Empty
dropJ n (Append _ l r) 
  | n < lSize = dropJ n l
  | otherwise = dropJ (n - lSize) r
  where lSize = getSize . size $ tag l

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty          = Empty
takeJ 0 _              = Empty
takeJ _ x@(Single _ _) = x
takeJ n (Append _ l r) 
  | n <= lSize = takeJ n l
  | otherwise = l +++ (takeJ (n - lSize) r)
  where lSize = getSize . size $ tag l

