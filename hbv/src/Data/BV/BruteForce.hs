module Data.BV.BruteForce (solve, allProgs) where

import Data.Maybe (catMaybes)

import Data.BV.Eval
import Data.BV.Types

------------------------------------------------------------------------

solve :: Problem -> Prog
solve = undefined

allProgs :: Problem -> [Prog]
allProgs (Problem sz ops ios) = [p | p <- ps, check p]
  where
    ps = map Prog (allExprs (sz-1) ops [X])
    check p = all (\(i,o) -> evalProg p i == o) ios

-- O(scary)
allExprs :: Size -> [Op] -> [Id] -> [Expr]
allExprs sz ops ids | sz <= 0   = []
                    | otherwise = filter ok (s1 ++ s2 ++ s3 ++ s4 ++ s5)
  where
    ok e = let sz' = exprSize e
               fs  = countFolds e
           in sz == sz' && (fs == 0 || fs == 1)

    s1 = [Zero, One] ++ map Id ids
    s2 = catMaybes [mop1  o e     | e <-  exprs 1, o <- ops]
    s3 = catMaybes [mop2  o l r   | l <-  exprs 2, r <-  exprs 2, o <- ops]
    s4 = catMaybes [mif   o p t f | p <-  exprs 3, t <-  exprs 3, f <-  exprs 3, o <- ops]
    s5 = catMaybes [mfold o a b c | a <- fexprs 4, b <- fexprs 4, c <- fexprs 4, o <- ops]

    exprs n  = allExprs (sz-n) ops ids
    fexprs n = allExprs (sz-n) ops (ids ++ [Y,Z])

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
mfold OTFold e0 e1 e2 = Just (Fold e0 e1 e2)
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
