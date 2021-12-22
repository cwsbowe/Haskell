{-# LANGUAGE DeriveGeneric #-}
--SKELETON FILE FOR HANDIN 1 OF COURSEWORK 1 for COMP2209, 2021
--CONTAINS ALL FUNCTIONS REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES
--Julian Rathke, Oct 2021

module Exercises (vigenere,frequency,amSplit) where

-- The following two imports are needed for testing, do not delete
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq

import Data.Char
import Data.List
import Data.Function

-- Exercise A1

-- import Data.Char
removeNonLetters :: String -> String
removeNonLetters "" = ""
removeNonLetters word = filter (`elem` ['a'..'z']++['A'..'Z']) word

encryptLetters :: (String, String) -> String
encryptLetters ("", "") = ""
encryptLetters (wordLetters, keyLetters) = encryptLetter (toUpper $ head wordLetters, toUpper $ head keyLetters) : encryptLetters (tail wordLetters, tail keyLetters)

encryptLetter :: (Char, Char) -> Char
encryptLetter (wordLetter, keyLetter) = chr $ (ord wordLetter + ord keyLetter - 130) `mod` 26 + 65

decryptLetters :: (String, String) -> String
decryptLetters ("", "") = ""
decryptLetters (cipherLetters, keyLetters) = decryptLetter (toUpper $ head cipherLetters, toUpper $ head keyLetters) : decryptLetters (tail cipherLetters, tail keyLetters)

decryptLetter :: (Char, Char) -> Char
decryptLetter (cipherLetter, keyLetter) = chr $ (ord cipherLetter - ord keyLetter + 26) `mod` 26 + 65

vigenere :: String -> (String -> String, String -> String)
vigenere "" = error "FAIL"
vigenere key = (\word -> encryptLetters (removeNonLetters word, removeNonLetters $ take (length $ removeNonLetters word) (cycle $ removeNonLetters key)), \cipher -> decryptLetters (removeNonLetters cipher, take (length $ removeNonLetters cipher) (cycle $ removeNonLetters key)))

-- Exercise A2

--import Data.List
--import Data.Function
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