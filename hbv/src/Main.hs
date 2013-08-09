{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -w #-}

module Main (main) where

import qualified Data.ByteString.Lazy.Char8 as B
import           Test.QuickCheck

import           Data.BV

------------------------------------------------------------------------

main :: IO ()
main = do
    quickCheck prop_encode_decode

prop_encode_decode p = case ep' of
    Left err -> error ("parse-failed: " ++ err ++ "\n" ++ B.unpack bs)
    Right p' -> p == p'
  where
    bs  = encodeProg p
    ep' = parseProg bs

