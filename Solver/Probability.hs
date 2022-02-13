module Solver.Probability
  ( getNextGuess
  ) where

import Data.Char
import Data.Foldable

data TileDist = TileDist [Float] deriving (Show)

getLetterProb :: TileDist -> Char -> Float
getLetterProb (TileDist dist) c = dist !! (ord c - ord 'a')

getDistAsList :: TileDist -> [Float]
getDistAsList (TileDist dist) = dist

-- Creates non-normalised distribution; you must divide by length afterwards
-- findTileDist current cs = [Float]
findTileDist :: [Float] -> [Char] -> [Float]
findTileDist current [] = current
findTileDist current (c:cs) = let
  i = ord (toLower c) - ord 'a'
  new = take i current ++ ((current !! i) + 1) : drop (i+1) current
  in findTileDist new cs

makeDists :: [String] -> [TileDist]
makeDists dict = let
  is = [0..4]
  all = map (\i -> map (!! i) dict) is
  dist = map ( map (/ fromIntegral (length dict)) . findTileDist [0.0 | _ <- ['a'..'z']] ) all
  in map TileDist dist

wordExpectation :: [TileDist] -> String -> Float
wordExpectation dists word = sum (map (\i -> getLetterProb (dists !! i) (word !! i)) [0..4])

-- find the most likely word in the dictionary
findMostLikely :: [String] -> (String, Float)
findMostLikely dict = let
  dists = makeDists dict
  wordProbs = map (\word -> (word, wordExpectation dists word)) dict
  in maximumBy (\x y -> compare (snd x) (snd y)) wordProbs

getNextGuess :: [String] -> String
getNextGuess dict = fst $ findMostLikely dict
