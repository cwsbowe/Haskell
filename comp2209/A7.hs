{-# LANGUAGE DeriveGeneric #-}
--SKELETON FILE FOR HANDIN 2 OF COURSEWORK 1 for COMP2209, 2020
--CONTAINS ALL FUNCTIONS REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES
--Julian Rathke, Oct 2020

module Exercises (Expr(..),toNNF) where

-- The following two  imports are needed for testing, do not delete
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq

-- Exercise A7
data Expr = Var Char | Not Expr | And Expr Expr | Or Expr Expr deriving (Eq, Ord)

--Define Expr as an instance of Show here:


instance Show Expr where
    show (Not (And a b)) = "~(" ++ show (And a b) ++ ")"
    show (Not (Or a b)) = "~(" ++ show (Or a b) ++ ")"
    
    show (And (And a b) (And c d)) = "(" ++ show (And a b) ++ ") ^ (" ++ show (And c d) ++ ")"
    show (And (And a b) (Or c d)) = "(" ++ show (And a b) ++ ") ^ (" ++ show (Or c d) ++ ")"
    show (And (Or a b) (And c d)) = "(" ++ show (Or a b) ++ ") ^ (" ++ show (And c d) ++ ")"
    show (And (Or a b) (Or c d)) = "(" ++ show (Or a b) ++ ") ^ (" ++ show (Or c d) ++ ")"

    show (And (And a b) c) = "(" ++ show (And a b) ++ ") ^ " ++ show c
    show (And a (And b c)) = show a ++ " ^ (" ++ show (And b c) ++ ")"
    show (And (Or a b) c) = "(" ++ show (Or a b) ++ ") ^ " ++ show c
    show (And a (Or b c)) = show a ++ " ^ (" ++ show (Or b c) ++ ")"
    
    show (Or (And a b) (And c d)) = "(" ++ show (And a b) ++ ") v (" ++ show (And c d) ++ ")"
    show (Or (And a b) (Or c d)) = "(" ++ show (And a b) ++ ") v (" ++ show (Or c d) ++ ")"
    show (Or (Or a b) (And c d)) = "(" ++ show (Or a b) ++ ") v (" ++ show (And c d) ++ ")"
    show (Or (Or a b) (Or c d)) = "(" ++ show (Or a b) ++ ") v (" ++ show (Or c d) ++ ")"
    
    show (Or (And a b) c) = "(" ++ show (And a b) ++ ") v " ++ show c
    show (Or a (And b c)) = show a ++ " v (" ++ show (And b c) ++ ")"
    show (Or (Or a b) c) = "(" ++ show (Or a b) ++ ") v " ++ show c
    show (Or a (Or b c)) = show a ++ " v (" ++ show (Or b c) ++ ")"
    
    show (Not a) = "~" ++ show a
    show (And a b) = show a ++ " ^ " ++ show b
    show (Or a b) = show a ++ " v " ++ show b
    show (Var a) = [a]


-- Define toNNF here:
toNNF :: Expr -> Expr
toNNF (Not (And a b)) = toNNF $ Or (Not a) (Not b)
toNNF (Not (Or a b)) = toNNF $ And (Not a) (Not b)
toNNF (Not (Not a)) = toNNF a
toNNF (And a b) = And (toNNF a) (toNNF b)
toNNF (Or a b) = Or (toNNF a) (toNNF b)
toNNF a = a