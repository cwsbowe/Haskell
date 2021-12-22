--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2021

--EXERCISE A5 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES


module Exercises (findBonding) where

import Data.List
import Data.Function
import Data.Maybe (isNothing)

-- Exercise A5
reverseTuple :: (a, a) -> (a,a)
reverseTuple tuple = (snd tuple, fst tuple)

listAll :: Eq a => [a] -> Int -> Int -> [(a, a)]
listAll [] check with = []
listAll xs check with | check == with = listAll xs check (with+1)
    | check < length xs && with < length xs = (xs!!check, xs!!with) : listAll xs check (with+1)
    | check >= length xs = []
    | otherwise = listAll xs (check+1) 0

filterListPred :: Eq a => (a -> a -> Bool) -> [(a, a)] -> [(a, a)]
filterListPred predicate [] = []
filterListPred predicate xs | uncurry predicate (head xs) = head xs : filterListPred predicate (tail xs)
    | otherwise = filterListPred predicate (tail xs)

listDups :: Eq a => [(a, a)] -> [(a, Int)]
listDups [] = []
listDups xs = (fst (head xs), countDups xs) : listDups (drop (countDups xs) xs)

countDups :: Eq a => [(a, a)] -> Int
countDups xs | length xs < 2 = 1
    | fst (head xs) == fst (xs!!1) = 1 + countDups (tail xs)
    | otherwise = 1

removeDups :: Eq a => [(a, a)] -> [(a, Int)] -> [(a, a)]
removeDups [] ys = []
removeDups xs [] = []
removeDups xs ys = findVal xs (fst (head ys)) : removeDups (removeOccurence xs (findVal xs (fst (head ys)))) (sortBy (compare `on` snd) (listDups (removeOccurence xs (findVal xs (fst (head ys))))))

removeOccurence :: Eq a => [(a, a)] -> (a, a) -> [(a, a)]
removeOccurence [] tuple = []
removeOccurence xs tuple | fst (head xs) == fst tuple || fst (head xs) == snd tuple || snd (head xs) == fst tuple || snd (head xs) == snd tuple = removeOccurence (tail xs) tuple
    | otherwise = head xs : removeOccurence (tail xs) tuple

notUsed :: Eq a => [(a, Int)] -> a -> Bool
notUsed [] value = False
notUsed ys value | fst (head ys) == value = True
    | otherwise = notUsed (tail ys) value

findVal :: Eq a => [(a, a)] -> a -> (a, a)
findVal xs value | fst (head xs) == value = head xs
    | otherwise = findVal (tail xs) value

findOther :: Eq a => [(a, Int)] -> a -> (a, Int)
findOther [] value = (value, 0)
findOther ys value | fst (head ys) == value = head ys
    | otherwise = findOther (tail ys) value

addReverse :: Eq a => [(a, a)] -> [(a, a)]
addReverse [] = []
addReverse xs = [head xs, reverseTuple (head xs)] ++ addReverse (tail xs)

assertPresent :: Eq a => [(a, a)] -> [a] -> Int -> Maybe [(a, a)]
assertPresent tuples [] count = Just tuples
assertPresent tuples values count | count >= length tuples = Nothing
    | head values == fst (tuples!!count) = assertPresent tuples (tail values) 0
    | otherwise = assertPresent tuples values (count+1)

findBonding :: Ord a => Eq a => (a -> a -> Bool) -> [a] -> Maybe [(a,a)]
findBonding predicate [] = Just []
findBonding predicate xs = assertPresent (addReverse $ removeDups (filterListPred predicate (listAll xs 0 1)) (sortBy (compare `on` snd) (listDups (filterListPred predicate (listAll xs 0 1))))) xs 0