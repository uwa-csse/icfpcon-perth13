{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -w #-}

module Main (main) where

import           Control.Applicative ((<$>))
import           Control.Monad (when)
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Word (Word64)
import           Numeric (showHex)
import           System.Console.CmdArgs

import           Data.BV
import           Data.BV.Test (checkProps)

------------------------------------------------------------------------

main :: IO ()
main = do
    args <- cmdArgs hbvArgs
    when (qc args) checkProps
    when (eval args) evalStdin

------------------------------------------------------------------------

data HbvArgs = HbvArgs {
    qc   :: Bool
  , eval :: Bool
  } deriving (Show, Data, Typeable)

hbvArgs = HbvArgs {
    qc   = False &= help "QuickCheck self test"
  , eval = False &= help "Evaluate program from stdin"
  } &= summary "HBV v0.1"

------------------------------------------------------------------------

evalStdin :: IO ()
evalStdin = do
    inp <- B.lines <$> B.getContents
    case inp of
      []     -> putStrLn "hbv: no input"
      (x:xs) -> case parseProg x of
        Left err -> putStrLn ("hbv: " ++ err)
        Right p  -> do
          putStrLn ("parsed: " ++ show p)
          mapM_ putStrLn $ map (go p . readWord64 . B.unpack) xs
  where
    go :: Prog -> Word64 -> String
    go p x = showWord64 x ++ " -> " ++ showWord64 (evalProg p x)

    showWord64 :: Word64 -> String
    showWord64 w = "0x" ++ showHex w ""

    readWord64 :: String -> Word64
    readWord64 = read
