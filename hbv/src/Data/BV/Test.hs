{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Data.BV.Test (
      checkProps
    , sampleProgs
    ) where

import           Control.Applicative ((<$>), (<*>), pure)
import qualified Data.ByteString.Lazy.Char8 as B
import           Test.QuickCheck (Gen, Arbitrary(..), sample', oneof, sized, quickCheck)

import           Data.BV.Parser
import           Data.BV.Types

------------------------------------------------------------------------

checkProps :: IO ()
checkProps = quickCheck prop_encode_decode

prop_encode_decode p = case ep' of
    Left err -> error ("parse-failed: " ++ err ++ "\n" ++ B.unpack bs)
    Right p' -> p == p'
  where
    bs  = encodeProg p
    ep' = parseProg bs

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
