{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -w #-}

module Main (main) where

import           Control.Monad (when)
import           Data.Data (Data)
import           Data.Typeable (Typeable)
import           System.Console.CmdArgs
import           Test.QuickCheck

import           Data.BV.Test (checkProps)

------------------------------------------------------------------------

main :: IO ()
main = do
    args <- cmdArgs hbvArgs
    when (qc args) checkProps

data HbvArgs = HbvArgs {
    qc :: Bool
  } deriving (Show, Data, Typeable)

hbvArgs = HbvArgs {
    qc = False &= help "QuickCheck"
  } &= summary "HBV v0.1"
