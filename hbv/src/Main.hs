{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -w #-}

module Main (main) where

import           Control.Applicative ((<$>))
import           Control.Monad (when)
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Word (Word64)
import           Numeric (showHex)
import           Prelude hiding (all)
import           System.Console.CmdArgs
import           System.IO (stdout, stderr, hPutStrLn)

import           Data.BV
import qualified Data.BV.BruteForce as BF
--import qualified Data.BV.BruterForce as BRF
import qualified Data.BV.SMT as SMT
import           Data.BV.Explore
import           Data.BV.SMTEval (progEquivInfo, exprEquivInfo)
import           Data.BV.Test (checkProps)

------------------------------------------------------------------------

main :: IO ()
main = do
    args <- cmdArgs hbvArgs
    case (perms args, hbvMode args) of
      (0, Eval)        -> evalStdin
      (0, SMT)         -> smtStdin (all args)
      (0, BruteForce)  -> bfStdin (all args)
      --(0, RBruteForce) -> brfStdin (all args)
      (0, Compare)     -> compareStdin
      (0, QC)          -> checkProps
      (depth, _)       -> findPerms depth

------------------------------------------------------------------------

data HbvArgs = HbvArgs {
    hbvMode :: HbvMode
  , all     :: Bool
  , perms   :: Int
  } deriving (Show, Data, Typeable)

data HbvMode = Eval | SMT | BruteForce | RBruteForce | Compare | QC
  deriving (Show, Data, Typeable)

hbvArgs = HbvArgs {
    hbvMode = enum [ Eval       &= help "Evaluate program from stdin (default)"
                   , SMT        &= help "Solve problem from stdin using SMT solver"
                   , BruteForce &= help "Solve problem from stdin using brute-force solver"
                   , RBruteForce &= help "Solve problem from stdin using bruter-force solver"
                   , Compare    &= help "Compare two programs from stdin for equivalence"
                   , QC         &= help "QuickCheck self test"
                   ]
  , all   = False &= help "Show all possible solutions instead of quitting after one"
  , perms = 0     &= help "Enumerate all functionally unique programs up to the specified depth"
  } &= summary "HBV v0.1"

------------------------------------------------------------------------

findPerms :: Int -> IO ()
findPerms depth = do
    mapM_ go (allExprs $ explore depth)
  where
    go (n, es) = do
      putStrLn ("# Size: " ++ show n)
      mapM_ (putStrLn . showExpr) es
      putStrLn ""

------------------------------------------------------------------------

evalStdin :: IO ()
evalStdin = do
    inp <- B.lines <$> B.getContents
    case inp of
      []     -> putStrLn "hbv: no input"
      (x:xs) -> case parse x of
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

bfStdin :: Bool -> IO ()
bfStdin = solverStdin BF.solve

--brfStdin :: Bool -> IO ()
--brfStdin = solverStdin BRF.solve

smtStdin :: Bool -> IO ()
smtStdin = solverStdin SMT.solve

solverStdin :: (Problem -> [Prog]) -> Bool -> IO ()
solverStdin solve showAll = do
    bs <- B.getContents
    let p = readProblem bs
        c = readComments bs
    hPutStrLn stdaux $ concat $ [
        "Size = ", show (pSize p)
      , ", Ops = ", show (pOps p)
      , ", IO = ", show (length (pIO p)), " pairs"
      ]
    hPutStrLn stdaux "======="
    let ps = solve p
    mapM_ (go c) $ if showAll then ps else take 1 ps
    hPutStrLn stdaux "======="
    B.hPutStr stdaux c
  where
    stdaux = stderr

    go c p = do
      putStrLn (showProg p)
      when ("(lambda" `B.isPrefixOf` c) $ do
        case parse (head $ B.lines c) of
          Left err -> hPutStrLn stdaux ("hbv: cannot parse challenge: " ++ err)
          Right cp -> hPutStrLn stdaux (progEquivInfo p cp)

------------------------------------------------------------------------

compareStdin :: IO ()
compareStdin = do
    putStrLn "compare"
    bs <- B.lines <$> B.getContents
    case bs of
      (b1:b2:_) -> do
        let e1 = parse' b1 "first expr"
            e2 = parse' b2 "second expr"
        putStrLn (exprEquivInfo e1 e2)
      _         -> hPutStrLn stderr "hbv: need two programs to compare"
  where
    parse' bs name = case parse bs of
      Left err -> error ("hbv: cannot parse " ++ name ++ ": " ++ err)
      Right p  -> p
