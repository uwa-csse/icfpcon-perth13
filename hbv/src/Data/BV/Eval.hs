module Data.BV.Eval (
      Vars(..)
    , evalProg
    , evalExpr
    ) where

import Data.Bits
import Data.Word (Word8, Word64)

import Data.BV.Types

------------------------------------------------------------------------

data Vars = Vars !Word64 !Word64 !Word64
  deriving (Show)

evalProg :: Prog -> Word64 -> Word64
evalProg (Prog e) x = evalExpr (Vars x 0 0) e

evalExpr :: Vars -> Expr -> Word64
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

evalExpr v (If0 p t f) = if evalExpr v p == 0
                         then evalExpr v t
                         else evalExpr v f

evalExpr v@(Vars x _ _) (Fold e0 e1 lam) = foldr f x0 xs
  where
    f :: Word64 -> Word64 -> Word64
    f y z = evalExpr (Vars x y z) lam

    xs :: [Word64]
    xs = map fromIntegral $ unpack64 (evalExpr v e0)

    x0 :: Word64
    x0 = evalExpr v e1

------------------------------------------------------------------------

unpack64 :: Word64 -> [Word8]
unpack64 x = [ fromIntegral (shiftR x 56)
             , fromIntegral (shiftR x 48)
             , fromIntegral (shiftR x 40)
             , fromIntegral (shiftR x 32)
             , fromIntegral (shiftR x 24)
             , fromIntegral (shiftR x 16)
             , fromIntegral (shiftR x  8)
             , fromIntegral x
             ]
