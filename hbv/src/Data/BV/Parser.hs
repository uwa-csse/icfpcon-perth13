{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.BV.Parser (
      parseProg
    , showProg
    , encodeProg
    , readProblem
    , readComments
    ) where

import           Control.Applicative ((<$>), (<*>), (<*), (<|>), pure)
import           Data.AttoLisp (Lisp(..), Result(..), FromLisp, ToLisp)
import           Data.AttoLisp (encode, toLisp, mkStruct)
import           Data.AttoLisp (lisp, fromLisp, parseLisp, struct, typeMismatch)
import qualified Data.Attoparsec.ByteString.Lazy as A
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.List (nub)
import           Data.String (IsString(..))
import qualified Data.Text as T

import           Data.BV.Types

------------------------------------------------------------------------
-- Parsing

parseProg :: B.ByteString -> Either String Prog
parseProg bs = case el of
    Left err -> Left err
    Right l  -> case fromLisp (fixIdents l) of
      Error err -> Left $ err ++ ": " ++ show (fixIdents l)
      Success p -> Right p
  where
    el :: Either String Lisp
    el = A.parseOnly (lisp <* A.endOfInput) (B.toStrict bs)

fixIdents :: Lisp -> Lisp
fixIdents l = replaceIdents (nub $ idents l) l
  where
    idents (Symbol x) | "x_" `T.isPrefixOf` x = [x]
    idents (List xs)                          = concatMap idents xs
    idents _                                  = []

replaceIdents :: [T.Text] -> Lisp -> Lisp
replaceIdents = go
  where
    go (x:_)     (Symbol s) | s == x = Symbol "x"
    go (_:y:_)   (Symbol s) | s == y = Symbol "y"
    go (_:_:z:_) (Symbol s) | s == z = Symbol "z"
    go vs        (List xs)           = List $ map (go vs) xs
    go _         x                   = x


instance IsString Prog where
    fromString s = case parseProg (B.pack s) of
      Left err -> error $ "fromString: cannot parse program: " ++ err
      Right p  -> p

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

    parseLisp (List [Symbol "fold", e0, e1, List [Symbol "lambda", List [Symbol "x", Symbol "y"], e2]]) =
        Fold <$> parseLisp e0 <*> parseLisp e1 <*> parseLisp (replaceIdents ["","x","y"] e2)

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
-- Parsing Problem/Ops

readOp :: B.ByteString -> Op
readOp "not"   = O1 Not
readOp "shl1"  = O1 Shl1
readOp "shr1"  = O1 Shr1
readOp "shr4"  = O1 Shr4
readOp "shr16" = O1 Shr16
readOp "and"   = O2 And
readOp "or"    = O2 Or
readOp "xor"   = O2 Xor
readOp "plus"  = O2 Plus
readOp "if0"   = OIf0
readOp "tfold" = OTFold
readOp "fold"  = OFold
readOp op      = error $ "readOp: unknown operator: " ++ B.unpack op

readProblem :: B.ByteString -> Problem
readProblem bs = case filterComments (B.lines bs) of
    []     -> error "readProblem: no input"
    (x:xs) -> prob x (map io xs)
  where
    prob line = case B.words line of
        (x:xs) -> Problem (read (B.unpack x)) (map readOp xs)
        _      -> error $ "readProblem: could not read size/ops: " ++ B.unpack line

    io line = case B.words line of
        (i:o:_) -> (read $ B.unpack i, read $ B.unpack o)
        _       -> error $ "readProblem: could not read input/output pair: " ++ B.unpack line

filterComments :: [B.ByteString] -> [B.ByteString]
filterComments = filter (not . ("#" `B.isPrefixOf`))

readComments :: B.ByteString -> B.ByteString
readComments = B.unlines . map clean . filter isComment . B.lines
  where
    clean = B.dropWhile (== ' ') . B.drop 1
    isComment = ("#" `B.isPrefixOf`)

------------------------------------------------------------------------
-- Pretty Printing

showProg :: Prog -> String
showProg = B.unpack . encodeProg

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
