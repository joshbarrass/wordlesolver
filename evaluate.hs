import Text.Printf
import Data.Foldable

import Common
import Solver.Probability

autoSolve :: [String] -> String -> [[Result]]
autoSolve [] _ = []
autoSolve dict word = let
  guess = getNextGuess dict
  result = check word guess
  newDict = filterDict guess result dict
  in if all (==Correct) result then [result] else result : autoSolve newDict word

mean :: (Foldable t, Real a, Fractional a) => t a -> a
mean xs = let
  in sum xs / fromIntegral (length xs)

main = do
  dict <- loadDictionary "words.txt"
  let sols = map (autoSolve dict) dict
  let solved = filter (all (==Correct) . last) sols
  let solvedInTime = filter ((<= 6) . length) solved
  let guessesNeeded = map length solvedInTime
  putStrLn $ "Total words: " ++ show (length dict)
  let percentSolved = fromIntegral (length solvedInTime) / fromIntegral (length dict) * 100 :: Double
  let meanGuesses = (mean $ map fromIntegral guessesNeeded) :: Double
  putStrLn $ "Number solved: " ++ show (length solvedInTime) ++ " (" ++ printf "%.2f" percentSolved ++ "%)"
  putStrLn $ "Average guesses needed: " ++ printf "%.2f" meanGuesses
  putStrLn $ "Max guesses needed: " ++ show (length $ maximumBy (\x y -> length x `compare` length y) solvedInTime)
  return ()
