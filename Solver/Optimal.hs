module Solver.Optimal
  ( getNextGuess
  , findWorstCase
  ) where

import Data.Foldable
import Solver.Common

findWorstCase :: [String] -> String -> Int
findWorstCase dict word = let
  rs = checkAll dict word
  binned = binResults rs
  in maximum binned

getNextGuess :: [String] -> [String] -> String
getNextGuess _ [] = ""
getNextGuess fullDict rem = let
  allWorstCase = map (\x -> (x, findWorstCase rem x)) fullDict
  bestWorstCase = minimumBy (\x y -> compare (snd x) (snd y)) allWorstCase
  -- if it is better to just guess a word, do so
  in if (length rem - 1) > snd bestWorstCase then fst bestWorstCase else head rem
