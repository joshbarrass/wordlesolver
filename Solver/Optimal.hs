module Solver.Optimal
  ( getNextGuess
  ) where

import Common
import Data.Foldable

allResults :: [[Result]]
allResults = let
  x = [Correct, Position, Wrong]
  in [[a,b,c,d,e] | a <- x, b <- x, c <- x, d <- x, e <- x]

findWorstCase :: [String] -> String -> Int
findWorstCase dict word = length $ maximumBy (\x y -> length x `compare` length y) $ map (\result -> filterDict word result dict) allResults

getNextGuess :: [String] -> String
getNextGuess [] = ""
getNextGuess dict = fst $ minimumBy (\x y -> compare (snd x) (snd y)) $ map (\x -> (x, findWorstCase dict x)) dict
