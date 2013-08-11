{-# LANGUAGE ParallelListComp #-}
{-# OPTIONS_GHC -w #-}

module Data.BV.Explore (
      HExprSet(..)
    , HExpr(..)
    , allExprs
    , explore
    ) where

import           Control.Parallel.Strategies (using, parListChunk, rseq)
import           Data.Bits
import qualified Data.HashMap.Strict as M
import           Data.Hashable
import           Data.Maybe (catMaybes, mapMaybe)
import           Data.StableMemo (memo2)
import qualified Data.Vector as V
import           Data.Word (Word64)
import           GHC.Conc (getNumCapabilities)
import           System.IO.Unsafe (unsafePerformIO)
import           System.Mem.StableName

import           Data.BV.Eval (Vars(..), evalExpr)
import           Data.BV.SMTEval (exprEquiv, exprEquivInfo)
import           Data.BV.Types
import           Data.BV.Parser (showExpr)

import           Debug.Trace

------------------------------------------------------------------------

data HExpr = SE {
    heHash :: !Int
  , heExpr :: !Expr
  } deriving (Show)

instance Hashable HExpr where
  hashWithSalt _ (SE h _) = h

instance Eq HExpr where
  (SE h0 e0) == (SE h1 e1) = h0 == h1 && e0 `exprEq` e1
    where
      exprEq e0 e1 | sn0 < sn1 = memoEquiv e0 e1
                   | otherwise = memoEquiv e1 e0

      sn0 = hashStableName (unsafePerformIO (makeStableName e0))
      sn1 = hashStableName (unsafePerformIO (makeStableName e1))

memoEquiv :: Expr -> Expr -> Bool
memoEquiv = memo2 go
  where
    go x y | exprEquiv x y = True
           | otherwise     = trace collis False
      where
        collis = "Z3 Collision:"
              ++ "\n  " ++ showExpr x
              ++ "\n  " ++ showExpr y
              ++ "\n" ++ exprEquivInfo x y
              ++ "\n"

hexpr :: Expr -> HExpr
hexpr e = SE hashO e
  where
    outputs = [(p + evalExpr v e) | (p, v) <- varInputs]
    hashO   = hash outputs

type Prime = Word64

varInputs :: [(Prime, Vars)]
varInputs = [(p, Vars x y z) | p <- primes | x <- inputs, y <- inputs, z <- inputs]
  where
    inputs = [ 0x0000000000000000
             , 0x0000000000000001
             , 0x000000000000000f
             , 0xffffffff00000000
             , 0xffffffffffffffff
             , 0xfffffffffffffff5
             , 0x1055555510333333
             ]

primes :: (Ord a, Enum a, Num a) => [a]
primes = 2 : eratos [3,5..]
  where
    eratos []     = []
    eratos (p:xs) = p : eratos (xs `minus` [p, p+2*p..])

    minus  xs    []     = xs
    minus (x:xs) (y:ys) = case (compare x y) of
        LT -> x : minus  xs  (y:ys)
        EQ ->     minus  xs     ys
        GT ->     minus (x:xs)  ys

    union  xs     []    = xs
    union  []     ys    = ys
    union (x:xs) (y:ys) = case (compare x y) of
        LT -> x : union  xs  (y:ys)
        EQ -> x : union  xs     ys
        GT -> y : union (x:xs)  ys

------------------------------------------------------------------------

data HExprSet = HExprSet {
    hsExprs      :: M.HashMap HExpr ()
  , hsSizedExprs :: M.HashMap Int (V.Vector Expr)
  }

insert :: Expr -> HExprSet -> HExprSet
insert e orig@(HExprSet es ses) = HExprSet es' ses'
  where
    h    = hexpr e
    es'  = M.insert h () es
    ses' = M.insertWith (V.++) sz (V.singleton e) ses
    sz   = exprSize e

member :: Expr -> HExprSet -> Bool
member e (HExprSet es _) = M.member (hexpr e) es

insertAll :: HExprSet -> [Expr] -> HExprSet
insertAll set es = foldr insert set (catMaybes mes)
  where
    n = length es
    mes = map go es `using` parListChunk (n `div` numCapabilities) rseq

    go e | member e set = Nothing
         | otherwise    = Just e

numCapabilities :: Int
numCapabilities = unsafePerformIO getNumCapabilities

depth :: Int -> HExprSet -> [[Expr]]
depth n (HExprSet _ ses) = map go [1..n]
  where
    go x = V.toList . M.lookupDefault V.empty x $ ses

empty :: HExprSet
empty = HExprSet M.empty M.empty

explore :: Int -> HExprSet
explore 0 = empty

explore 1 = insertAll empty [Zero, One, Id X, Id Y, Id Z]

explore n = insertAll set $ op1s n set ++ op2s n set ++ if0 n set
  where
    set = explore (n-1)

allExprs :: HExprSet -> [(Int, [Expr])]
allExprs (HExprSet _ ses) = map (\(i,v) -> (i, V.toList v)) (M.toList ses)

------------------------------------------------------------------------

op1s :: Int -> HExprSet -> [Expr]
op1s n set = concatMap (op1 n set) [Not, Shl1, Shr1, Shr4, Shr16]

op2s :: Int -> HExprSet -> [Expr]
op2s n set = concatMap (op2 n set) [And, Or, Xor, Plus]

op1 :: Int -> HExprSet -> Op1 -> [Expr]
op1 n set o = [ Op1 o e
              | es <- depth n set
              , e <- es ]

op2 :: Int -> HExprSet -> Op2 -> [Expr]
op2 n set o = [ Op2 o e1 e2
              | (n1, es1) <- zip [1..] $ depth (n-2) set
              , es2 <- depth (n-n1-1) set
              , e1 <- es1
              , e2 <- es2 ]

if0 :: Int -> HExprSet -> [Expr]
if0 n set = [ If0 e1 e2 e3
            | (n1, es1) <- zip [1..] $ depth (n-3) set
            , (n2, es2) <- zip [1..] $ depth (n-n1-2) set
            , es3 <- depth (n-n1-n2-1) set
            , e1 <- es1
            , e2 <- es2
            , e3 <- es3
            ]
