module Solver.Optimal
  ( getNextGuess
  ) where

import Common
import Data.Foldable
import Data.Map (Map, fromList, adjust)

checkAll :: [String] -> String -> [[Result]]
checkAll dict word = map (`check` word) dict

resultIndex :: [Result] -> Int
resultIndex xs = sum [(case x of
                             Correct -> 0
                             Position -> 1
                             Wrong -> 2
                  ) * (3^i) | (i, x) <- zip [0..] xs]

binResults :: [[Result]] -> Map Int Int
binResults [] = fromList [(i, 0) | i <- [0.. (resultIndex [Wrong, Wrong, Wrong, Wrong, Wrong])]]
binResults (x:xs) = let
  i = resultIndex x
  in adjust (+1) i (binResults xs)

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
