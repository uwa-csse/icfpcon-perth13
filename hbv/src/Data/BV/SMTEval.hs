module Data.BV.SMTEval (
      Vars(..)
    , progEquiv
    , exprEquiv
    , progEquivInfo
    , exprEquivInfo
    , evalProg
    , evalExpr
    ) where

import Data.Bits
import Data.SBV
import System.IO.Unsafe (unsafePerformIO)

import Data.BV.Types

------------------------------------------------------------------------

data Vars = Vars !SWord64 !SWord64 !SWord64
  deriving (Show)

------------------------------------------------------------------------

progEquiv :: Prog -> Prog -> Bool
progEquiv p0 p1 = unsafePerformIO (progEquivIO isUnsat p0 p1)

exprEquiv :: Expr -> Expr -> Bool
exprEquiv e0 e1 = unsafePerformIO (exprEquivIO isUnsat e0 e1)

progEquivInfo :: Prog -> Prog -> String
progEquivInfo p0 p1 = unsafePerformIO (progEquivIO show p0 p1)

exprEquivInfo :: Expr -> Expr -> String
exprEquivInfo e0 e1 = unsafePerformIO (exprEquivIO show e0 e1)

progEquivIO :: (ThmResult -> a) -> Prog -> Prog -> IO a
progEquivIO f p0 p1 = f `fmap` proveWith z3 equiv
  where
    equiv = forAll ["x"] $ \x -> evalProg p0 x .== evalProg p1 x

exprEquivIO :: (ThmResult -> a) -> Expr -> Expr -> IO a
exprEquivIO f e0 e1 = f `fmap` proveWith z3 equiv
  where
    equiv = forAll ["x", "y", "z"] $ \x y z ->
        let v = Vars x y z
        in evalExpr v e0 .== evalExpr v e1

isUnsat :: ThmResult -> Bool
isUnsat t = case t of
  ThmResult (Unsatisfiable _) -> True
  ThmResult (Satisfiable _ _) -> False
  _                           -> error $ "isUnsat: " ++ show t

------------------------------------------------------------------------

evalProg :: Prog -> SWord64 -> SWord64
evalProg (Prog e) x = evalExpr (Vars x 0 0) e

evalExpr :: Vars -> Expr -> SWord64
evalExpr (Vars x _ _) (Id X) = x
evalExpr (Vars _ y _) (Id Y) = y
evalExpr (Vars _ _ z) (Id Z) = z
evalExpr _ Zero              = 0
evalExpr _ One               = 1

evalExpr v (Op1 Not   e)  = complement (evalExpr v e)
evalExpr v (Op1 Shl1  e)  = evalExpr v e `shiftL` 1
evalExpr v (Op1 Shr1  e)  = evalExpr v e `shiftR` 1
evalExpr v (Op1 Shr4  e)  = evalExpr v e `shiftR` 4
evalExpr v (Op1 Shr16 e)  = evalExpr v e `shiftR` 16
evalExpr v (Op2 Plus l r) = evalExpr v l + evalExpr v r
evalExpr v (Op2 And  l r) = evalExpr v l .&. evalExpr v r
evalExpr v (Op2 Or   l r) = evalExpr v l .|. evalExpr v r
evalExpr v (Op2 Xor  l r) = evalExpr v l `xor` evalExpr v r

evalExpr v (If0 p t f) = ite (evalExpr v p .== 0)
                             (evalExpr v t)
                             (evalExpr v f)

evalExpr v@(Vars x _ _) (Fold e0 e1 lam) = foldr f x0 xs
  where
    f :: SWord64 -> SWord64 -> SWord64
    f y z = evalExpr (Vars x y z) lam

    xs :: [SWord64]
    xs = map (extend.extend.extend) $ unpack64 (evalExpr v e0)

    x0 :: SWord64
    x0 = evalExpr v e1

------------------------------------------------------------------------

unpack64 :: SWord64 -> [SWord8]
unpack64 w64 =
    [w8_0, w8_1, w8_2, w8_3, w8_4, w8_5, w8_6, w8_7]
  where
    (w32_0, w32_1) = split w64

    (w16_0, w16_1) = split w32_0
    (w16_2, w16_3) = split w32_1

    (w8_0, w8_1) = split w16_0
    (w8_2, w8_3) = split w16_1
    (w8_4, w8_5) = split w16_2
    (w8_6, w8_7) = split w16_3
