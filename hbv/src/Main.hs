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
import qualified Data.BV.SMT as SMT
import qualified Data.BV.BruteForce as BF
import           Data.BV.Test (checkProps)

------------------------------------------------------------------------

main :: IO ()
main = do
    args <- cmdArgs hbvArgs
    case hbvMode args of
      Eval       -> evalStdin
      SMT        -> smtStdin
      BruteForce -> bfStdin
      QC         -> checkProps

------------------------------------------------------------------------

data HbvArgs = HbvArgs {
    hbvMode  :: HbvMode
  } deriving (Show, Data, Typeable)

data HbvMode = Eval | SMT | BruteForce | QC
  deriving (Show, Data, Typeable)

hbvArgs = HbvArgs {
    hbvMode = enum [ Eval       &= help "Evaluate program from stdin (default)"
                   , SMT        &= help "Solve problem from stdin using SMT solver"
                   , BruteForce &= help "Solve problem from stdin using brute-force solver"
                   , QC         &= help "QuickCheck self test"
                   ]
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
          putStrLn ("size: " ++ show (progSize p))
          mapM_ putStrLn $ map (go p . readWord64 . B.unpack) xs
  where
    go :: Prog -> Word64 -> String
    go p x = showWord64 x ++ " -> " ++ showWord64 (evalProg p x)

    showWord64 :: Word64 -> String
    showWord64 w = "0x" ++ showHex w ""

    readWord64 :: String -> Word64
    readWord64 = read

------------------------------------------------------------------------

bfStdin :: IO ()
bfStdin = do
    bs <- B.getContents
    let p = readProblem bs
        c = readComments bs
    putStrLn $ concat $ [
        "Size = ", show (pSize p)
      , ", Ops = ", show (pOps p)
      , ", IO = ", show (length (pIO p)), " pairs"
      ]
    putStrLn "======="
    mapM_ (putStrLn . showProg) (BF.allProgs p)
    putStrLn "======="
    B.putStr c

------------------------------------------------------------------------

smtStdin :: IO ()
smtStdin = do
    p <- readProblem <$> B.getContents
    print p
    putStrLn "======="
    print (SMT.solve p)
