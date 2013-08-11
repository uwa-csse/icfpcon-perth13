module Data.BV.Explore (
      SubExpr(..)
    , explore
    ) where

import Data.List (union)
import Data.Word (Word64)

import Data.BV.Eval (Vars(..), evalExpr)
import Data.BV.SMTEval (progEquiv)
import Data.BV.Types

------------------------------------------------------------------------

data SubExpr = SE {
      seExpr   :: Expr
    , seIds    :: [Id]
    , seOutput :: [Word64]
    } deriving (Eq, Show)

------------------------------------------------------------------------

explore :: Int -> [[SubExpr]]
explore 0 = []
explore 1 = [[ subE Zero   []
             , subE One    []
             , subE (Id X) [X]
             --, subE (Id Y) [Y]
             --, subE (Id Z) [Z]
             ]]
explore n = es ++ [cull (concat es) $ op1s n es ++ op2s n es ++ if0 n es]
  where
    es = explore (n-1)

------------------------------------------------------------------------

op1s :: Int -> [[SubExpr]] -> [SubExpr]
op1s n ess = concatMap (op1 n ess) [Not, Shl1, Shr1, Shr4, Shr16]

op2s :: Int -> [[SubExpr]] -> [SubExpr]
op2s n ess = concatMap (op2 n ess) [And, Or, Xor, Plus]

op1 :: Int -> [[SubExpr]] -> Op1 -> [SubExpr]
op1 n ess o = [ subE (Op1 o e) i
              | es <- take (n-1) ess
              , (SE e i _) <- es ]

op2 :: Int -> [[SubExpr]] -> Op2 -> [SubExpr]
op2 n ess o = [ subE (Op2 o e1 e2) (i1 `union` i2)
              | (n1, es1) <- zip [1..] $ take (n-2) ess
              , es2 <- take (n-n1-1) ess
              , (SE e1 i1 _) <- es1
              , (SE e2 i2 _) <- es2 ]

if0 :: Int -> [[SubExpr]] -> [SubExpr]
if0 n ess = [ subE (If0 e1 e2 e3) (i1 `union` i2 `union` i3)
            | (n1, es1) <- zip [1..] $ take (n-3) ess
            , (n2, es2) <- zip [1..] $ take (n-n1-2) ess
            , es3 <- take (n-n1-n2-1) ess
            , (SE e1 i1 _) <- es1
            , (SE e2 i2 _) <- es2
            , (SE e3 i3 _) <- es3
            ]

------------------------------------------------------------------------

cull :: [SubExpr] -> [SubExpr] -> [SubExpr]
cull existExprs newExprs = go newExprs existExprs
  where
    go []     _  = []
    go (e:es) [] = [e] ++ go es [e]
    go (e:es) xs | any (`exprEquiv` e) xs = go es xs
                 | otherwise              = [e] ++ go es (e:xs)

exprEquiv :: SubExpr -> SubExpr -> Bool
exprEquiv (SE e1 _ o1) (SE e2 _ o2) =
    --o1 == o2 && qed
    o1 == o2
    -- False
  where
    qed = Prog e1 `progEquiv` Prog e2

subE :: Expr -> [Id] -> SubExpr
subE e ids = SE e ids $ map eval inputs
  where
    eval x = evalExpr (Vars x 0 0) e

    -- TODO: Better numbers
    inputs = [ 0x0000000000000000
             , 0x0000000000000001
             , 0x00000000000000ff
             , 0x000000000000ff00
             , 0x000000000000ffff
             , 0x00000000ffff0000
             , 0x00000000ffffffff
             , 0xffffffff00000000
             , 0xffffffffffffffff
             , 0xfffffffffffffffe
             , 0xf0f0f0f0f0f0f0f0
             , 0x0f0f0f0f0f0f0f0f
             , 0xff00ff00ff00ff00
             , 0x00ff00ff00ff00ff
             , 0xffff0000ffff0000
             , 0x0000ffff0000ffff
             ]
