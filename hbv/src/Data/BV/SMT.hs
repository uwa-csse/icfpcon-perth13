{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
    satProg :: [[Op]]
    satProg = unsafePerformIO $ do
      m <- sat prog
      case getModel m of
        Right (True, _)   -> error "hbv: Backend solver reported \"unknown\""
        Right (False, xs) -> return [map w2o xs]
        Left _            -> return []

    prog :: Symbolic SBool
    prog = do
      (eops :: [SOp]) <- map SOp <$> mkExistVars (sz-1)
      constrain $ bAll (`sElem` ops') eops
      constrain $ bAll (`sElem` eops) ops'
      return true

    ops' = map o2s ops

------------------------------------------------------------------------


newtype SOp = SOp { unSOp :: SWord8 }
  deriving (EqSymbolic, Num)

w2o :: Word8 -> Op
w2o 0  = (O1 Not)
w2o 1  = (O1 Shl1)
w2o 2  = (O1 Shr1)
w2o 3  = (O1 Shr4)
w2o 4  = (O1 Shr16)
w2o 5  = (O2 And)
w2o 6  = (O2 Or)
w2o 7  = (O2 Xor)
w2o 8  = (O2 Plus)
w2o 9  = OIf0
w2o 10 = OTFold
w2o 11 = OFold
w2o x  = error $ "hbv: w2o: unknown operation: " ++ show x

o2s :: Op -> SOp
o2s (O1 Not)   = 0
o2s (O1 Shl1)  = 1
o2s (O1 Shr1)  = 2
o2s (O1 Shr4)  = 3
o2s (O1 Shr16) = 4
o2s (O2 And)   = 5
o2s (O2 Or)    = 6
o2s (O2 Xor)   = 7
o2s (O2 Plus)  = 8
o2s OIf0       = 9
o2s OTFold     = 10
o2s OFold      = 11
