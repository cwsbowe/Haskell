{-# LANGUAGE DeriveGeneric #-}
--SKELETON FILE FOR HANDIN 2 OF COURSEWORK 1 for COMP2209, 2020
--CONTAINS ALL FUNCTIONS REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES
--Julian Rathke, Oct 2020

module Exercises (neighbours,findBonding,Position(..), Board(..), label, empty, newboard, moves, slide) where

-- The following two  imports are needed for testing, do not delete
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
-- Put ALL of your own import statements here:
import Data.List
import Data.Function



-- Exercise A4
type Point a = (a,a)
type Metric a = (Point a) -> (Point a) -> Double

applyMetric :: Metric a -> Point a -> [Point a] -> [(Point a, Double)]
applyMetric d p [] = []
applyMetric d p xs = (head xs, d p (head xs)) : applyMetric d p (tail xs)

removeDistances :: [(Point a, Double)] -> [Point a]
removeDistances [] = []
removeDistances points_distances = fst (head points_distances) : removeDistances (tail points_distances)

neighbours :: Int -> Metric a -> Point a -> [Point a] -> [Point a]
neighbours k d p xs | k >= 0 = take k $ removeDistances $ sortBy (compare `on` snd) $ applyMetric d p xs
    | otherwise = error "Negative value k"

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

-- Exercise A6
-- Do not modify this datatype
data Position = NW | N | NE | W | M | E | SW | S | SE
    deriving (Eq, Ord, Show)

-- Provide your own definition of the Board datatype here 
data Board a = Board
    {
        tiles :: [(a, Position)]
    }

-- Define all of the board functions below:
newboard :: [a] -> Board a
newboard tiles | length tiles /= 8 = error "wrong length"
    | otherwise = Board [(head tiles, NW), (tiles!!1, N), (tiles!!2, NE), (tiles!!3, W), (tiles!!4, M), (tiles!!5, E), (tiles!!6, SW), (tiles!!7, S)]

checkPosition :: Position -> [(a, Position)] -> Maybe a
checkPosition pos [] = Nothing
checkPosition pos tiles | snd (head tiles) == pos = Just $ fst (head tiles)
    | otherwise = checkPosition pos (tail tiles)

label :: Position -> Board a -> Maybe a
label pos boa = checkPosition pos (tiles boa)

checkEmpty :: [(a, Position)] -> [Position] -> Position
checkEmpty [] positions = head positions
checkEmpty tiles positions = checkEmpty (tail tiles) (delete (snd (head tiles)) positions)

empty :: Board a -> Position
empty boa = checkEmpty (tiles boa) [NW, N, NE, W, M, E, SW, S, SE]

moves :: Board a -> [Position]
moves boa | empty boa == NW = [N, W]
    | empty boa == N = [NW, NE, M]
    | empty boa == NE = [N, E]
    | empty boa == W = [NW, M, SW]
    | empty boa == M = [N, W, E, S]
    | empty boa == E = [NE, M, SE]
    | empty boa == SW = [W, S]
    | empty boa == S = [M, SW, SE]
    | empty boa == SE = [E, S]

replaceBoard :: Position -> Position -> [(a, Position)] -> [(a, Position)]
replaceBoard pos emp [] = []
replaceBoard pos emp tiles | snd (head tiles) == pos = (fst (head tiles), emp) : replaceBoard pos emp (tail tiles)
    | otherwise = head tiles : replaceBoard pos emp (tail tiles)

slide :: Position -> Board a -> Board a
slide pos boa | pos `elem` moves boa = Board $ replaceBoard pos (empty boa) (tiles boa)
    | otherwise = error "space not adjacent"
