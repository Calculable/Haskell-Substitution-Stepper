{-# LANGUAGE GADTs #-}
{-# OPTIONS -XNoImplicitPrelude #-}

module GADTSourceExample where

import SteppablePrelude

data Expr a where
  ILit :: Int -> Expr Int
  BLit :: Bool -> Expr Bool
  AddExpr :: Expr Int -> Expr Int -> Expr Int
  EqExpr :: Eq a => Expr a -> Expr a -> Expr Bool

evalExpr :: Expr a -> a
evalExpr (ILit x) = x
evalExpr (BLit x) = x
evalExpr (AddExpr a b) = evalExpr a + evalExpr b
evalExpr (EqExpr a b) = evalExpr a == evalExpr b
