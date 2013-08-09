{-# LANGUAGE OverloadedStrings #-}

module Data.BV (
      Prog(..)
    , Expr(..)
    , Id(..)
    , Op1(..)
    , Op2(..)

    , parseProg
    , encodeProg
    , sampleProgs
    ) where

import           Control.Applicative ((<$>), (<*>), (<*), (<|>), pure)
import           Data.AttoLisp (Lisp(..), Result(..), FromLisp, ToLisp)
import           Data.AttoLisp (lisp, fromLisp, parseLisp, struct, typeMismatch)
import           Data.AttoLisp (encode, toLisp, mkStruct)
import qualified Data.Attoparsec.ByteString.Lazy as A
import qualified Data.ByteString.Lazy.Char8 as B

import           Test.QuickCheck (Gen, Arbitrary(..), sample', oneof, sized)

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
-- Parsing

parseProg :: B.ByteString -> Either String Prog
parseProg bs = case el of
    Left err -> Left err
    Right l  -> case fromLisp l of
      Error err -> Left err
      Success p -> Right p
  where
    el :: Either String Lisp
    el = A.parseOnly (lisp <* A.endOfInput) (B.toStrict bs)

instance FromLisp Prog where
    parseLisp = struct "lambda" prog
      where
        prog :: [Id] -> Expr -> Prog
        prog _ e = Prog e

instance FromLisp Expr where
    parseLisp (Number 0) = pure Zero
    parseLisp (Number 1) = pure One

    parseLisp (List [s@(Symbol _), e]) =
        Op1 <$> parseLisp s <*> parseLisp e

    parseLisp (List [s@(Symbol _), l, r]) =
        Op2 <$> parseLisp s <*> parseLisp l <*> parseLisp r

    parseLisp (List [Symbol "fold", e0, e1, List [Symbol "lambda", List [Symbol "y", Symbol "z"], e2]]) =
        Fold <$> parseLisp e0 <*> parseLisp e1 <*> parseLisp e2

    parseLisp e = Id <$> parseLisp e
              <|> struct "if0" If0 e
              <|> typeMismatch "Expr" e

instance FromLisp Id where
    parseLisp (Symbol "x") = pure X
    parseLisp (Symbol "y") = pure Y
    parseLisp (Symbol "z") = pure Z
    parseLisp e = typeMismatch "Id" e

instance FromLisp Op1 where
    parseLisp (Symbol "not")   = pure Not
    parseLisp (Symbol "shl1")  = pure Shl1
    parseLisp (Symbol "shr1")  = pure Shr1
    parseLisp (Symbol "shr4")  = pure Shr4
    parseLisp (Symbol "shr16") = pure Shr16
    parseLisp e = typeMismatch "Op1" e

instance FromLisp Op2 where
    parseLisp (Symbol "and")  = pure And
    parseLisp (Symbol "or")   = pure Or
    parseLisp (Symbol "xor")  = pure Xor
    parseLisp (Symbol "plus") = pure Plus
    parseLisp e = typeMismatch "Op2" e

------------------------------------------------------------------------
-- Pretty Printing

encodeProg :: Prog -> B.ByteString
encodeProg = encode

instance ToLisp Prog where
    toLisp (Prog e) = mkStruct "lambda" [List [Symbol "x"], toLisp e]

instance ToLisp Expr where
    toLisp Zero            = Number 0
    toLisp One             = Number 1
    toLisp (Id x)          = toLisp x
    toLisp (If0 p t f)     = mkStruct "if0" [toLisp p, toLisp t, toLisp f]
    toLisp (Op1 o e)       = List [toLisp o, toLisp e]
    toLisp (Op2 o l r)     = List [toLisp o, toLisp l, toLisp r]
    toLisp (Fold e0 e1 e2) = mkStruct "fold" [toLisp e0, toLisp e1, lambda]
      where
        lambda = mkStruct "lambda" [List [Symbol "y", Symbol "z"], toLisp e2]

instance ToLisp Op1 where
    toLisp Not   = Symbol "not"
    toLisp Shl1  = Symbol "shl1"
    toLisp Shr1  = Symbol "shr1"
    toLisp Shr4  = Symbol "shr4"
    toLisp Shr16 = Symbol "shr16"

instance ToLisp Op2 where
    toLisp And  = Symbol "and"
    toLisp Or   = Symbol "or"
    toLisp Xor  = Symbol "xor"
    toLisp Plus = Symbol "plus"

instance ToLisp Id where
    toLisp X = Symbol "x"
    toLisp Y = Symbol "y"
    toLisp Z = Symbol "z"

------------------------------------------------------------------------
-- Arbitrary

sampleProgs :: IO [Prog]
sampleProgs = sample' arbitrary

instance Arbitrary Prog where
    arbitrary = Prog <$> arbitrary

instance Arbitrary Expr where
    arbitrary = sized arbExpr

arbExpr :: Int -> Gen Expr
arbExpr 0 = oneof [pure Zero, pure One, Id <$> arbitrary]
arbExpr n = oneof [
      pure Zero
    , pure One
    , Id   <$> arbitrary
    , If0  <$> expr <*> expr <*> expr
    , Op1  <$> arbitrary <*> expr
    , Op2  <$> arbitrary <*> expr <*> expr
    , Fold <$> expr <*> expr <*> expr
    ]
  where
    expr = arbExpr (n `div` 2)

instance Arbitrary Op1 where
    arbitrary = oneof [
          pure Not
        , pure Shl1
        , pure Shr1
        , pure Shr4
        , pure Shr16
        ]

instance Arbitrary Op2 where
    arbitrary = oneof [
          pure And
        , pure Or
        , pure Xor
        , pure Plus
        ]

instance Arbitrary Id where
    arbitrary = oneof [pure X, pure Y, pure Z]

------------------------------------------------------------------------
-- Docs

-- 0. Syntax
--
--  program    P ::= "(" "lambda" "(" id ")" e ")"
--  expression e ::= "0" | "1" | id
--                | "(" "if0" e e e ")"
--                | "(" "fold" e e "(" "lambda" "(" id id ")" e ")" ")"
--                | "(" op1 e ")"
--                | "(" op2 e e ")"
--           op1 ::= "not" | "shl1" | "shr1" | "shr4" | "shr16"
--           op2 ::= "and" | "or" | "xor" | "plus"
--           id  ::= [a-z]+
--
-- A valid program P contains at most one occurrence of "fold".
-- The only constants in a source program are 0 and 1.
-- However, \BV programs can be evaluated on arbitrary 64-bit values.
