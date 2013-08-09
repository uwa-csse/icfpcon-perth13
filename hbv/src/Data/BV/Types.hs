{-# LANGUAGE OverloadedStrings #-}

module Data.BV.Types (
      Prog(..)
    , Expr(..)
    , Id(..)
    , Op1(..)
    , Op2(..)
    ) where

------------------------------------------------------------------------
-- Types

data Prog = Prog Expr
  deriving (Eq, Show)

data Expr = Zero
          | One
          | Id Id
          | If0  Expr Expr Expr
          | Fold Expr Expr Expr
          | Op1 Op1 Expr
          | Op2 Op2 Expr Expr
  deriving (Eq, Show)

data Id = X | Y | Z
  deriving (Eq, Show)

data Op1 = Not | Shl1 | Shr1 | Shr4 | Shr16
  deriving (Eq, Show)

data Op2 = And | Or | Xor | Plus
  deriving (Eq, Show)
