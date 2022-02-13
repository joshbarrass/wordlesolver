module Solver.HardOptimal
  ( getNextGuess
  ) where

import Data.Foldable
import Solver.Common

findWorstCase :: [String] -> String -> Int
findWorstCase dict word = let
  rs = checkAll dict word
  binned = binResults rs
  in maximum binned


getNextGuess :: [String] -> String
getNextGuess [] = ""
getNextGuess dict = let
  allWorstCase = map (\x -> (x, findWorstCase dict x)) dict
  in fst $ minimumBy (\x y -> compare (snd x) (snd y)) allWorstCase
