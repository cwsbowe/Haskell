{-# LANGUAGE DeriveGeneric #-}
--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2021

--EXERCISE A3 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES


module Exercises (amSplit) where

-- The following two imports are needed for testing, do not delete
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq

-- Exercise A3
splitOne :: Ord a => [a] -> [a]
splitOne list | length list < 3 = list
    | head list > list!!1 = head list : list!!1 : checkDesc (drop 2 list) (list!!1)
    | head list < list!!1 = head list : list!!1 : checkAsc (drop 2 list) (list!!1)
    | otherwise = head list : splitOne (tail list)

checkAsc :: Ord a => [a] -> a -> [a]
checkAsc list element | null list = []
    | head list > element = []
    | head list < element = head list : checkDesc (tail list) (head list)
    | otherwise = head list : checkAsc (tail list) (head list)

checkDesc :: Ord a => [a] -> a -> [a]
checkDesc list element | null list = []
    | head list > element = head list : checkAsc (tail list) (head list)
    | head list < element = []
    | otherwise = head list : checkDesc (tail list) (head list)

amSplit :: Ord a => [a] -> [[a]]
amSplit [] = []
amSplit list = splitOne list : amSplit (drop (length (splitOne list)) list)

--amSplit xs = undefined
