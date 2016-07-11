{-# OPTIONS_GHC -Wall #-}

module Scrabble where

import Data.Monoid

newtype Score = Score Int
   deriving (Eq, Show)

getScore :: Score -> Int
getScore (Score x) = x

instance Monoid Score where
   mempty = Score 0
   mappend (Score a) (Score b) = Score ( a + b )

score :: Char -> Score
score c
   | elem c ['A', 'a','E', 'e', 'F', 'f', 'L', 'l', 'N', 'n', 'O', 'o', 'R',
            'r', 'S', 's', 'T', 't', 'U', 'u'] = Score 1
   | elem c ['D', 'd', 'G', 'g'] = Score 2
   | elem c ['B', 'b', 'C', 'c', 'M', 'm', 'P', 'p'] = Score 3
   | elem c ['F', 'f', 'H', 'h', 'V', 'v', 'W', 'w', 'Y', 'y'] = Score 4
   | elem c ['K', 'k'] = Score 5
   | elem c ['J', 'j', 'X', 'x'] = Score 8
   | elem c ['Q', 'q', 'Z', 'z'] = Score 10
   | otherwise = Score 0

scoreString :: String -> Score
scoreString = foldr (\ x y -> score x `mappend` y) mempty
