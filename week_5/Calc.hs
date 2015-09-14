{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
module Calc where
import ExprT
import Parser (parseExp)
import qualified StackVM as SVM
import qualified Data.Map as M

{- Exercise 1 -}

eval :: ExprT -> Integer
eval (Lit x)   = x
eval (Add x y) = (eval x) + (eval y)
eval (Mul x y) = (eval x) * (eval y)

{- Exercise 2 -}

evalStr :: String -> Maybe Integer
evalStr str = maybeval $ parseExp Lit Add Mul str
  where maybeval Nothing = Nothing
        maybeval (Just x) = Just (eval x)

{- Exercise 3 -}

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit x   = Lit x
  add x y = Add x y
  mul x y = Mul x y

reify :: ExprT -> ExprT
reify = id

{- Exercise 4 -}

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (>0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
  lit                       = MinMax
  add (MinMax x) (MinMax y) = lit $ max x y
  mul (MinMax x) (MinMax y) = lit $ min x y

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
  lit x                 = Mod7 $ x `mod` 7
  add (Mod7 x) (Mod7 y) = lit (x + y)
  mul (Mod7 x) (Mod7 y) = lit (x + y)

{- Exercise 5 -}

instance Expr SVM.Program where
  lit x = [SVM.PushI x]
  add x y = x ++ y ++ [SVM.Add]
  mul x y = x ++ y ++ [SVM.Mul]

compile :: String -> Maybe SVM.Program
compile = parseExp lit add mul

