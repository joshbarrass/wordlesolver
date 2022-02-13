import Data.Char

import Common
import Solver.Optimal

readResult :: String -> [Result]
readResult [] = []
readResult (x:xs)
  | l == 'O' = Correct : readResult xs
  | l == '?' = Position : readResult xs
  | otherwise = Wrong : readResult xs
  where l = toUpper x

solve :: [String] -> [String] -> String -> Int -> IO ()
solve _ _ _ 6 = putStr "Sorry, out of guesses :(\n"
solve dict rem guess n = do
  putStr $ "Guess \"" ++ guess ++ "\" (" ++ show (length rem) ++ " words remain)\n"
  putStr "Enter Result:\n"
  stringResult <- getLine
  -- word not accepted, try again
  case stringResult of
    "/" -> (do
               putStr "Skipping...\n"
               let newRem = removeFirst guess rem
               let newGuess = getNextGuess dict newRem
               solve dict newRem newGuess n
           )
    stringResult -> (do
                     let result = readResult stringResult
                     if result == [Correct, Correct, Correct, Correct, Correct]
                       then (do
                                putStr "Solved! :)\n"
                                return ()
                            )
                       else (do
                                let newRem = filterDict guess result rem
                                let newGuess = getNextGuess dict newRem
                                solve dict newRem newGuess (n+1)
                            )
                 )

main = do
  putStr "How to use:\nEnter the guess instructed and then input the result as follows:\n  grey = X or .\n  yellow = ?\n  green = O\nIf the word is rejected as invalid, just enter /\n\n"
  dict <- loadDictionary "words.txt"
  solve dict dict (getNextGuess dict dict) 0
