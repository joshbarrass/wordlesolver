import Text.Printf
import Data.Foldable

import Common
import Solver.Optimal

autoSolve :: [String] -> [String] -> Int -> String -> [[Result]]
autoSolve _ [] _ _ = []
autoSolve dict rem 1 word = let
  guess = "arise"
  result = check word guess
  newRem = filterDict guess result rem
  in if all (==Correct) result then [result] else result : autoSolve dict newRem 2 word
autoSolve dict rem i word = let
  guess = getNextGuess dict rem 
  result = check word guess
  newRem = filterDict guess result rem
  in if all (==Correct) result then [result] else result : autoSolve dict newRem (i+1) word

main = do
  dict <- loadDictionary "words.txt"
  let sols = map (\x -> (x, autoSolve dict dict 1 x)) dict
  let solved = filter (all (==Correct) . last . snd) sols
  let solvedInTime = filter ((<= 6) . length . snd) solved
  let guessesNeeded = map (length . snd) solvedInTime
  putStrLn $ "Total words: " ++ show (length dict)
  let percentSolved = fromIntegral (length solvedInTime) / fromIntegral (length dict) * 100 :: Double
  let meanGuesses = (mean $ map fromIntegral guessesNeeded) :: Double
  putStrLn $ "Number solved: " ++ show (length solvedInTime) ++ " (" ++ printf "%.2f" percentSolved ++ "%)"
  putStrLn $ "Average guesses needed: " ++ printf "%.2f" meanGuesses
  putStrLn $ "Max guesses needed: " ++ show (length $ maximumBy (\x y -> length x `compare` length y) solvedInTime)

  let solvedWords = map fst solvedInTime
  let failureWords = filter (`notElem` solvedWords) dict
  print failureWords
  
  return ()
