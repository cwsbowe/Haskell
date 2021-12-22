{-# LANGUAGE DeriveGeneric #-}

--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2021

--EXERCISE A6 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES

module Exercises (Position(..), Board(..), label, empty, newboard, moves, slide) where

-- The following two imports are needed for testing, do not delete
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq 
-- Put ALL of your own import statements here:
import Data.List
import Data.Maybe


--
-- Do not modify this datatype
data Position = NW | N | NE | W | M | E | SW | S | SE
    deriving (Eq, Ord, Show)


-- Exercise A6
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
