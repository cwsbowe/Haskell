{-# LANGUAGE DeriveGeneric #-}
--TEMPLATE FILE FOR COURSEWORK 1 for COMP2209
--Julian Rathke, Oct 2021

--EXERCISE A1 ONLY

--CONTAINS FUNCTION REQIURED FOR COMPILATION AGAINST THE TEST SUITE
--MODIFY THE FUNCTION DEFINITIONS WITH YOUR OWN SOLUTIONS
--IMPORTANT : DO NOT MODIFY ANY FUNCTION TYPES

module Exercises (vigenere) where

-- The following two imports are needed for testing, do not delete

import GHC.Generics (Generic,Generic1)
import Control.DeepSeq


-- Exercise A1
import Data.Char

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
