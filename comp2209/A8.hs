{-# LANGUAGE DeriveGeneric #-}
--SKELETON FILE FOR HANDIN 2 OF COURSEWORK 1 for COMP2209, 2020
--CONTAINS ALL FUNCTIONS REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES
--Julian Rathke, Oct 2020

module Exercises (Expr(..),Binding,Interpretation,consistent,solve,satisfiable) where

-- The following two  imports are needed for testing, do not delete
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
import Data.Foldable (minimumBy)
import Data.Function (on)
import Data.List (sortBy)

-- Exercise A8
data Expr = Var Char | Not Expr | And Expr Expr | Or Expr Expr deriving (Eq, Ord)
type Binding = (Char, Bool)
type Interpretation = [Binding]

consistent :: Interpretation -> Bool
consistent interps | length interps < 2 = True
    | fst (minimumBy (compare `on` fst) interps) == fst (sortBy (compare `on` fst) interps!!1) && snd (minimumBy (compare `on` fst) interps) /= snd (sortBy (compare `on` fst) interps!!1) = False
    | otherwise = consistent (tail (sortBy (compare `on` fst) interps))


solve :: Expr -> [Interpretation]
solve (And (And a b) (And c d)) = compoundAdd (solve (And a b)) (solve (And c d)) 0
solve (And (And a b) (Or c d)) = compoundAdd (solve (And a b)) (solve (Or c d)) 0
solve (And (Or a b) (And c d)) =  compoundAdd (solve (Or a b)) (solve (And c d)) 0
solve (And (Or a b) (Or c d)) = compoundAdd (solve (Or a b)) (solve (Or c d)) 0

solve (And (And a b) c) = compoundAdd (solve (And a b)) (solve c) 0
solve (And (Or a b) c) = compoundAdd (solve (Or a b)) (solve c) 0
solve (And a (And b c)) = compoundAdd (solve a) (solve (And b c)) 0
solve (And a (Or b c)) = compoundAdd (solve a) (solve (Or b c)) 0

solve (Or (And a b) (And c d)) = solve (And a b) ++ solve (And c d)
solve (Or (And a b) (Or c d)) = solve (And a b) ++ solve (Or c d)
solve (Or (Or a b) (And c d)) = solve (Or a b) ++ solve (And c d)
solve (Or (Or a b) (Or c d)) = solve (Or a b) ++ solve (Or c d)

solve (Or (And a b) c) = solve (And a b) ++ solve c
solve (Or (Or a b) c) = solve (Or a b) ++ solve c
solve (Or a (And b c)) = solve a ++ solve (And b c)
solve (Or a (Or b c)) = solve a ++ solve (Or b c)

solve (And a b) | solve a == solve b = solve a
    | consistent [head (head (solve a)), head (head (solve b))] = [[head (head (solve a)), head (head (solve b))]]
    | otherwise =  []
solve (Or a b) | head (head (solve a)) /= head (head (solve b)) = [[head (head (solve a))], [head (head (solve b))]]
    | otherwise = solve a
solve (Not (Var a)) = [[(a, False)]]
solve (Var a) = [[(a, True)]]
solve any = solve (toNNF any)

toNNF :: Expr -> Expr
toNNF (Not (And a b)) = toNNF $ Or (Not a) (Not b)
toNNF (Not (Or a b)) = toNNF $ And (Not a) (Not b)
toNNF (Not (Not a)) = toNNF a
toNNF (And a b) = And (toNNF a) (toNNF b)
toNNF (Or a b) = Or (toNNF a) (toNNF b)
toNNF a = a

compoundAdd :: [Interpretation] -> [Interpretation] -> Int -> [Interpretation]
compoundAdd [] interps1 n = []
compoundAdd interps0 [] n = []
compoundAdd interps0 interps1 n | (n+1) > length interps1 = compoundAdd (tail interps0) interps1 0
    | consistent (head interps0 ++ interps1!!n) = (head interps0 ++ interps1!!n) : compoundAdd interps0 interps1 (n+1)
    | otherwise = compoundAdd interps0 interps1 (n+1)


satisfiable :: [Expr] -> Bool
satisfiable [] = True
satisfiable exprs | not (null (interpssToInterps $ exprsToInterpss exprs)) = True
    | otherwise = False

exprsToInterpss :: [Expr] -> [[Interpretation]]
exprsToInterpss [] = []
exprsToInterpss exprs = solve (head exprs) : exprsToInterpss (tail exprs)

interpssToInterps :: [[Interpretation]] -> [Interpretation]
interpssToInterps interps | length interps < 2 = head interps
interpssToInterps interpss = interpssToInterps (compoundAdd (head interpss) (interpss!!1) 0 : drop 2 interpss)