module Solver.Optimal
  ( getNextGuess
  , resultIndex
  , findWorstCase
  ) where

import Common
import Data.Foldable

allResults :: [[Result]]
allResults = let
  x = [Correct, Position, Wrong]
  in [[a,b,c,d,e] | a <- x, b <- x, c <- x, d <- x, e <- x]

-- findWorstCase :: [String] -> String -> Int
-- findWorstCase dict word = let
--   allFiltered = map (\result -> filterDict word result dict) allResults
--   in length $ maximumBy (\x y -> length x `compare` length y) allFiltered

checkAll :: [String] -> String -> [[Result]]
checkAll dict word = map (`check` word) dict

resultIndex :: [Result] -> Int
resultIndex xs = sum [(case x of
                             Correct -> 0
                             Position -> 1
                             Wrong -> 2
                  ) * (3^i) | (i, x) <- zip [0..] xs]

binResults :: [[Result]] -> [Int]
binResults [] = [0 | _ <- [0.. (resultIndex [Wrong, Wrong, Wrong, Wrong, Wrong])]]
binResults (x:xs) = let
  p = binResults xs
  i = resultIndex x
  xi = p !! i
  in take i p ++ (xi + 1) : drop (i+1) p

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
