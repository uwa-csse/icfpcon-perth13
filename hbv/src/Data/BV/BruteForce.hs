module Data.BV.BruteForce (solve, allProgs) where

import           Data.List (sort, nub, nubBy)
import           Data.Maybe (catMaybes)

import qualified Data.BV.Eval as E
import           Data.BV.SMTEval (progEquiv)
import           Data.BV.Types

------------------------------------------------------------------------

solve :: Problem -> Prog
solve = undefined

allProgs :: Problem -> [Prog]
allProgs (Problem sz ops ios) =
    nubBy progEquiv . filter checkEval . filter checkOps . filter checkSize $ ps
  where
    ps = map Prog (allExprs (sz-1) ops [X])

    checkEval p = all (\(i,o) -> E.evalProg p i == o) ios

    checkSize p = progSize p == sz

    checkOps (Prog e) = findOps e `opsEq` ops

-- O(scary)
allExprs :: Size -> [Op] -> [Id] -> [Expr]
allExprs sz ops ids | sz <= 0   = []
                    | otherwise = es'
  where
    es  = filter ok $ s1 ++ s2 ++ s3 ++ s4 ++ s5
    es' = nubBy exprEquiv es

    ok e = let sz' = exprSize e
               fs  = countFolds e
           in sz >= sz' && (fs == 0 || fs == 1)

    s1 = [Zero, One] ++ map Id ids
    s2 = catMaybes [mop1  o e     | e <- exprs 1, o <- ops]
    s3 = catMaybes [mop2  o l r   | l <- exprs 2, r <- exprs 2, l <= r, o <- ops]
    s4 = catMaybes [mif   o p t f | p <- exprs 3, t <- exprs 3, f <- exprs 3, o <- ops]
    s5 = catMaybes [mfold o a b c | a <- fxprs 4, b <- fxprs 4, c <- fxprs 4, o <- ops]

    exprs n = allExprs (sz-n) ops ids
    fxprs n = allExprs (sz-n) ops (ids ++ [Y,Z])

exprEquiv :: Expr -> Expr -> Bool
exprEquiv e0 e1 | eq        = True
                | otherwise = False
  where
    eval = E.evalExpr (E.Vars 0 0 0)

    eq = exprSize e0 == exprSize e1
      && noVars e0
      && noVars e1
      && eval e0 == eval e1
      && findOps e0 `opsEq` findOps e1

mop1 :: Op -> Expr -> Maybe Expr
mop1 (O1 o) e = Just (Op1 o e)
mop1 _ _      = Nothing

mop2 :: Op -> Expr -> Expr -> Maybe Expr
mop2 (O2 o) l r = Just (Op2 o l r)
mop2 _ _ _      = Nothing

mif :: Op -> Expr -> Expr -> Expr -> Maybe Expr
mif OIf0 p t f = Just (If0 p t f)
mif _ _ _ _    = Nothing

mfold :: Op -> Expr -> Expr -> Expr -> Maybe Expr
mfold OFold  e0 e1 e2 = Just (Fold e0 e1 e2)
mfold _ _ _ _         = Nothing

countFolds :: Expr -> Int
countFolds Zero            = 0
countFolds One             = 0
countFolds (Id _)          = 0
countFolds (Op1 _ e0)      = countFolds e0
countFolds (Op2 _ e0 e1)   = countFolds e0 + countFolds e1
countFolds (If0 e0 e1 e2)  = countFolds e0 + countFolds e1 + countFolds e2
countFolds (Fold e0 e1 e2) = 1 + countFolds e0 + countFolds e1 + countFolds e2

noVars :: Expr -> Bool
noVars Zero            = True
noVars One             = True
noVars (Id _)          = False
noVars (Op1 _ e0)      = noVars e0
noVars (Op2 _ e0 e1)   = noVars e0 && noVars e1
noVars (If0 e0 e1 e2)  = noVars e0 && noVars e1 && noVars e2
noVars (Fold e0 e1 e2) = noVars e0 && noVars e1 && noVars e2

findOps :: Expr -> [Op]
findOps Zero            = []
findOps One             = []
findOps (Id _)          = []
findOps (Op1 o e)       = [O1 o] ++ findOps e
findOps (Op2 o e0 e1)   = [O2 o] ++ findOps e0 ++ findOps e1
findOps (If0 e0 e1 e2)  = [OIf0] ++ findOps e0 ++ findOps e1 ++ findOps e2
findOps (Fold e0 e1 e2) = [OFold] ++ findOps e0 ++ findOps e1 ++ findOps e2

opsEq :: [Op] -> [Op] -> Bool
opsEq xs ys = sort (nub xs) == sort (nub ys)
