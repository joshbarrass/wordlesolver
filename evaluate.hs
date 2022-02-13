import Text.Printf
import Data.Foldable

import Common
import Solver.HardOptimal

autoSolve :: [String] -> Int -> String -> [[Result]]
autoSolve [] _ _ = []
autoSolve dict 1 word = let
  guess = "arise"
  result = check word guess
  -- newDict = filterDict guess result dict
  newDict = dict
  in if all (==Correct) result then [result] else result : autoSolve newDict 2 word
autoSolve dict i word = let
  guess = getNextGuess dict 
  result = check word guess
  -- newDict = filterDict guess result dict
  newDict = dict
  in if all (==Correct) result then [result] else result : autoSolve newDict (i+1) word

mean :: (Foldable t, Real a, Fractional a) => t a -> a
mean xs = let
  in sum xs / fromIntegral (length xs)

main = do
  dict <- loadDictionary "words.txt"
  let sols = map (\x -> (x, autoSolve dict 1 x)) dict
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
