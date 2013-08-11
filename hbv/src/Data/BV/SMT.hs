{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -w #-}

module Data.BV.SMT (solve) where

import Control.Applicative ((<$>))
import Data.SBV hiding (solve)
import System.IO.Unsafe (unsafePerformIO)

import Data.BV.Types

import Debug.Trace

------------------------------------------------------------------------

solve :: Problem -> [Prog]
solve (Problem sz ops ios) = traceShow satProg [Prog Zero]
  where
    satProg :: [[Code]]
    satProg = unsafePerformIO $ do
      m <- satWith z3 { timing = True, smtFile = Just "smt.txt" } prog
      case getModel m of
        Right (True, _)   -> error "hbv: backend solver reported \"unknown\""
        Right (False, xs) -> return xs
        Left err          -> error $ "hbv: " ++ err

    prog :: Symbolic SBool
    prog = do
      (cs :: [SCode]) <- mkExistVars n
      constrain $ bAll (`sElem` ps) cs
      constrain $ bAll (`sElem` cs) rs
      return $ bAll (correct cs) sios

    correct cs (i, o) = let (valid, o') = eval cs i
                        in valid &&& o .== o'

    sios = map (\(i,o) -> (literal i, literal o)) ios

    rs = required ops
    ps = possible ops

    -- fold counts as two instructions
    n | folded ops = sz - 2
      | otherwise  = sz - 1

------------------------------------------------------------------------
-- Constraints

eval :: [SCode] -> SWord64 -> (SBool, SWord64)
eval prog x = go prog []
  where
    valid r = (true, r)
    invalid = (false, 0)

    go :: [SCode] -> [SWord64] -> (SBool, SWord64)

    go [] []     = invalid -- invalid: stack has nothing on it
    go [] (r:[]) = valid r -- stack with single value to return
    go [] _      = invalid -- invalid: stack has more than one value

    go (c:cs) xs =
      ite (c .== s'0) (go cs $ 0 : xs) $
      ite (c .== s'1) (go cs $ 1 : xs) $
      ite (c .== s'x) (go cs $ x : xs) $

      if null xs then invalid else
      let u   = head xs
          xs' = tail xs in

      ite (c .== s'not)   (go cs $ complement u : xs') $
      ite (c .== s'shl1)  (go cs $ shiftL u 1   : xs') $
      ite (c .== s'shr1)  (go cs $ shiftR u 1   : xs') $
      ite (c .== s'shr4)  (go cs $ shiftR u 4   : xs') $
      ite (c .== s'shr16) (go cs $ shiftR u 16  : xs') $

      if null xs' then invalid else
      let v    = head xs'
          xs'' = tail xs' in

      ite (c .== s'and)  (go cs $ (u  .&.  v) : xs'') $
      ite (c .== s'or)   (go cs $ (u  .|.  v) : xs'') $
      ite (c .== s'xor)  (go cs $ (u `xor` v) : xs'') $
      ite (c .== s'plus) (go cs $ (u   +   v) : xs'') $

      if null xs'' then invalid else
      let w     = head xs''
          xs''' = tail xs'' in

      ite (c .== s'if0) (go cs $ ite (u .== 0) v w : xs''') $

      invalid


possible :: [Op] -> [SCode]
possible ops | folded ops = fold
             | otherwise  = base
  where
    base = required ops ++ map i2s [I'0, I'1, I'X]
    fold = base ++ map i2s [I'Y, I'Z]

required :: [Op] -> [SCode]
required ops = map o2s ops

folded :: [Op] -> Bool
folded ops = OTFold `elem` ops || OFold `elem` ops

------------------------------------------------------------------------
-- Types

data Instr
    = I'0
    | I'1
    | I'X
    | I'Y
    | I'Z
    | I'Not
    | I'Shl1
    | I'Shr1
    | I'Shr4
    | I'Shr16
    | I'And
    | I'Or
    | I'Xor
    | I'Plus
    | I'If0
    | I'Fold
  deriving (Eq, Ord)

type SCode = SBV Code

newtype Code = Code { unCode :: Word8 }
  deriving (Eq, Ord, HasKind, SymWord, SatModel)

c'0, c'1, c'x, c'y, c'z :: Code
c'not, c'shl1, c'shr1, c'shr4, c'shr16 :: Code
c'and, c'or, c'xor, c'plus, c'if0, c'fold :: Code
c'0     = Code 0
c'1     = Code 1
c'x     = Code 2
c'y     = Code 3
c'z     = Code 4
c'not   = Code 5
c'shl1  = Code 6
c'shr1  = Code 7
c'shr4  = Code 8
c'shr16 = Code 9
c'and   = Code 10
c'or    = Code 11
c'xor   = Code 12
c'plus  = Code 13
c'if0   = Code 14
c'fold  = Code 15

s'0, s'1, s'x, s'y, s'z :: SCode
s'not, s'shl1, s'shr1, s'shr4, s'shr16 :: SCode
s'and, s'or, s'xor, s'plus, s'if0, s'fold :: SCode
s'0     = literal c'0
s'1     = literal c'1
s'x     = literal c'x
s'y     = literal c'y
s'z     = literal c'z
s'not   = literal c'not
s'shl1  = literal c'shl1
s'shr1  = literal c'shr1
s'shr4  = literal c'shr4
s'shr16 = literal c'shr16
s'and   = literal c'and
s'or    = literal c'or
s'xor   = literal c'xor
s'plus  = literal c'plus
s'if0   = literal c'if0
s'fold  = literal c'fold

------------------------------------------------------------------------
-- Conversion

instance Show Code where
    show = show . c2i

instance Show Instr where
    show I'0     = "0"
    show I'1     = "1"
    show I'X     = "x"
    show I'Y     = "y"
    show I'Z     = "z"
    show I'Not   = "not"
    show I'Shl1  = "shl1"
    show I'Shr1  = "shr1"
    show I'Shr4  = "shr4"
    show I'Shr16 = "shr16"
    show I'And   = "and"
    show I'Or    = "or"
    show I'Xor   = "xor"
    show I'Plus  = "plus"
    show I'If0   = "if0"
    show I'Fold  = "fold"

i2s :: Instr -> SCode
i2s = literal . i2c

o2s :: Op -> SCode
o2s = literal . o2c

o2c :: Op -> Code
o2c = i2c . o2i

i2c :: Instr -> Code
i2c I'0     = c'0
i2c I'1     = c'1
i2c I'X     = c'x
i2c I'Y     = c'y
i2c I'Z     = c'z
i2c I'Not   = c'not
i2c I'Shl1  = c'shl1
i2c I'Shr1  = c'shr1
i2c I'Shr4  = c'shr4
i2c I'Shr16 = c'shr16
i2c I'And   = c'and
i2c I'Or    = c'or
i2c I'Xor   = c'xor
i2c I'Plus  = c'plus
i2c I'If0   = c'if0
i2c I'Fold  = c'fold

c2i :: Code -> Instr
c2i c = case unCode c of
    0  -> I'0
    1  -> I'1
    2  -> I'X
    3  -> I'Y
    4  -> I'Z
    5  -> I'Not
    6  -> I'Shl1
    7  -> I'Shr1
    8  -> I'Shr4
    9  -> I'Shr16
    10 -> I'And
    11 -> I'Or
    12 -> I'Xor
    13 -> I'Plus
    14 -> I'If0
    15 -> I'Fold
    x  -> error ("c2i: unknown instruction: " ++ show x)

o2i :: Op -> Instr
o2i (O1 Not)   = I'Not
o2i (O1 Shl1)  = I'Shl1
o2i (O1 Shr1)  = I'Shr1
o2i (O1 Shr4)  = I'Shr4
o2i (O1 Shr16) = I'Shr16
o2i (O2 And)   = I'And
o2i (O2 Or)    = I'Or
o2i (O2 Xor)   = I'Xor
o2i (O2 Plus)  = I'Plus
o2i OIf0       = I'If0
o2i OTFold     = I'Fold
o2i OFold      = I'Fold
