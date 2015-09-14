module ExprT where

data ExprT = Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT
  deriving (Show, Eq)

data VarExprT = VarLit Integer
              | Var String
              | VarAdd VarExprT VarExprT
              | VarMul VarExprT VarExprT
  deriving (Show, Eq)
