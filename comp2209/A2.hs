{-# LANGUAGE DeriveGeneric #-}
--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2021

--EXERCISE A2 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES


module Exercises (frequency) where

-- The following two imports are needed for testing, do not delete
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq



-- Exercise A2
import Data.List
import Data.Function
list :: Int -> String -> Int -> Int -> String
list n ct point plus | point * n + plus >= length ct = []
    | otherwise = (ct!!(n*point+plus)) : list n ct (point+1) plus

frequencies :: String -> Char -> Int -> (Char, Int)
frequencies nthchars letter point | point >= length nthchars = (letter, 1)
    | otherwise = if nthchars!!point == letter then (letter, 1 + snd (frequencies nthchars letter (point + 1))) else (letter, snd $ frequencies nthchars letter (point + 1))

listtotuple :: String -> [(Char, Int)]
listtotuple "" = []
listtotuple c = sortBy (flip compare `on` snd) $ sort $ frequencies c (head c) 1 : listtotuple (filter (`notElem` [head c]) c)

lists :: Int -> String -> Int -> [String]
lists n ct plus | plus < n = list n ct 0 plus : lists n ct (plus + 1)
    | otherwise = []

convertlists :: [String] -> Int -> [[(Char, Int)]]
convertlists ls point | point < length ls = listtotuple (ls!!point) : convertlists ls (point+1)
    | otherwise = []

frequency :: Int -> String -> [[(Char,Int)]]
frequency n ct = convertlists (lists n ct 0) 0
--frequency _ _ = [[(' ',0)]]