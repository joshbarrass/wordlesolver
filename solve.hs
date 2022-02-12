import Data.Char

import Common
import Solver

readResult :: String -> [Result]
readResult [] = []
readResult (x:xs)
  | l == 'O' = Correct : readResult xs
  | l == '?' = Position : readResult xs
  | otherwise = Wrong : readResult xs
  where l = toUpper x

solve :: [String] -> String -> Int -> IO ()
solve _ _ 6 = putStr "Sorry, out of guesses :(\n"
solve dict guess n = do
  putStr $ "Guess \"" ++ guess ++ "\" (" ++ show (length dict) ++ " words remain)\n"
  putStr "Enter Result:\n"
  stringResult <- getLine
  -- word not accepted, try again
  case stringResult of
    "/" -> (do
               putStr "Skipping...\n"
               let newDict = removeFirst guess dict
               let newGuess = getNextGuess newDict
               solve newDict newGuess n
           )
    stringResult -> (do
                     let result = readResult stringResult
                     if result == [Correct, Correct, Correct, Correct, Correct]
                       then (do
                                putStr "Solved! :)\n"
                                return ()
                            )
                       else (do
                                let newDict = filterDict guess result dict
                                let newGuess = getNextGuess newDict
                                solve newDict newGuess (n+1)
                            )
                 )

main = do
  putStr "How to use:\nEnter the guess instructed and then input the result as follows:\n  grey = X or .\n  yellow = ?\n  green = O\nIf the word is rejected as invalid, just enter /\n\n"
  dict <- loadDictionary "words.txt"
  solve dict (getNextGuess dict) 0
