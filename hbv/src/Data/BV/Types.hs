{-# LANGUAGE OverloadedStrings #-}

module Data.BV.Types (
      Prog(..)
    , Expr(..)
    , Id(..)
    , Op1(..)
    , Op2(..)
    , Problem(..)
    , Size
    , Op(..)
    , progSize
    , exprSize
    ) where

import Data.Word (Word64)

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

------------------------------------------------------------------------

data Problem = Problem {
      pSize :: Size
    , pOps  :: [Op]
    , pIO   :: [(Word64, Word64)]
    } deriving (Eq, Show)

type Size = Int

data Op = O1 Op1 | O2 Op2 | OIf0 | OTFold | OFold
  deriving (Eq, Show)

------------------------------------------------------------------------
-- Functions

progSize :: Prog -> Size
progSize (Prog e) = 1 + exprSize e

exprSize :: Expr -> Size
exprSize Zero            = 1
exprSize One             = 1
exprSize (Id _)          = 1
exprSize (If0 e0 e1 e2)  = 1 + exprSize e0 + exprSize e1 + exprSize e2
exprSize (Fold e0 e1 e2) = 2 + exprSize e0 + exprSize e1 + exprSize e2
exprSize (Op1 _ e0)      = 1 + exprSize e0
exprSize (Op2 _ e0 e1)   = 1 + exprSize e0 + exprSize e1
