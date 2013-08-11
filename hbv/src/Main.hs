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
import qualified Data.BV.SMT as SMT
import           Data.BV.Explore
import           Data.BV.SMTEval (progEquivInfo)
import           Data.BV.Test (checkProps)

------------------------------------------------------------------------

main :: IO ()
main = do
    args <- cmdArgs hbvArgs
    case (perms args, hbvMode args) of
      (depth, _)      -> findPerms depth
      (_, Eval)       -> evalStdin
      (_, SMT)        -> smtStdin (all args)
      (_, BruteForce) -> bfStdin (all args)
      (_, Compare)    -> compareStdin
      (_, QC)         -> checkProps

------------------------------------------------------------------------

data HbvArgs = HbvArgs {
    hbvMode :: HbvMode
  , all     :: Bool
  , perms   :: Int
  } deriving (Show, Data, Typeable)

data HbvMode = Eval | SMT | BruteForce | Compare | QC
  deriving (Show, Data, Typeable)

hbvArgs = HbvArgs {
    hbvMode = enum [ Eval       &= help "Evaluate program from stdin (default)"
                   , SMT        &= help "Solve problem from stdin using SMT solver"
                   , BruteForce &= help "Solve problem from stdin using brute-force solver"
                   , Compare    &= help "Compare two programs from stdin for equivalence"
                   , QC         &= help "QuickCheck self test"
                   ]
  , all   = False &= help "Show all possible solutions instead of quitting after one"
  , perms = 0     &= help "Enumerate all functionally unique programs up to the specified depth"
  } &= summary "HBV v0.1"

------------------------------------------------------------------------

findPerms :: Int -> IO ()
findPerms depth = do
    mapM_ go (zip [1..] $ explore depth)
  where
    go (n, es) = do
      putStrLn ("# Size: " ++ show n)
      mapM_ (putStrLn . showExpr . seExpr) es
      putStrLn ""

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

bfStdin :: Bool -> IO ()
bfStdin = solverStdin BF.solve

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
        case parseProg (head $ B.lines c) of
          Left err -> hPutStrLn stdaux ("hbv: cannot parse challenge: " ++ err)
          Right cp -> hPutStrLn stdaux (progEquivInfo p cp)

------------------------------------------------------------------------

compareStdin :: IO ()
compareStdin = do
    bs <- B.lines <$> B.getContents
    case bs of
      (b1:b2:_) -> do
        let p1 = parse b1 "first program"
            p2 = parse b2 "second program"
        putStrLn (progEquivInfo p1 p2)
      _         -> hPutStrLn stderr "hbv: need two programs to compare"
  where
    parse bs name = case parseProg bs of
      Left err -> error ("hbv: cannot parse " ++ name ++ ": " ++ err)
      Right p  -> p
