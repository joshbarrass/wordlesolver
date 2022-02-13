module Solver.Common
  ( checkAll
  , resultIndex
  , binResults
  ) where

import Data.Map (Map, fromList, adjust)

import Common

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
