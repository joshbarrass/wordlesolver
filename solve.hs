import Data.Char
import Data.Foldable

import Common

data TileDist = TileDist [Float] deriving (Show)

getLetterProb :: TileDist -> Char -> Float
getLetterProb (TileDist dist) c = dist !! (ord c - ord 'a')

getDistAsList :: TileDist -> [Float]
getDistAsList (TileDist dist) = dist

-- Creates non-normalised distribution; you must divide by length afterwards
-- findTileDist current cs = [Float]
findTileDist :: [Float] -> [Char] -> [Float]
findTileDist current [] = current
findTileDist current (c:cs) = let
  i = ord (toLower c) - ord 'a'
  new = take i current ++ ((current !! i) + 1) : drop (i+1) current
  in findTileDist new cs

makeDists :: [String] -> [TileDist]
makeDists dict = let
  is = [0..4]
  all = map (\i -> map (!! i) dict) is
  dist = map ( map (/ fromIntegral (length dict)) . findTileDist [0.0 | _ <- ['a'..'z']] ) all
  in map TileDist dist

wordExpectation :: [TileDist] -> String -> Float
wordExpectation dists word = sum (map (\i -> getLetterProb (dists !! i) (word !! i)) [0..4])

-- find the most likely word in the dictionary
findMostLikely :: [String] -> (String, Float)
findMostLikely dict = let
  dists = makeDists dict
  wordProbs = map (\word -> (word, wordExpectation dists word)) dict
  in maximumBy (\x y -> compare (snd x) (snd y)) wordProbs

-- checks whether a word is compatible with a result
isPossible :: String -> [Result] -> String -> Bool
isPossible guess result true = check true guess == result

-- filterDict :: String -> [Result] -> [String] -> [String]
filterDict guess result = filter (isPossible guess result) 

readResult :: String -> [Result]
readResult [] = []
readResult (x:xs)
  | l == 'O' = Correct : readResult xs
  | l == '?' = Position : readResult xs
  | otherwise = Wrong : readResult xs
  where l = toUpper x

getNextGuess :: [String] -> String
getNextGuess dict = fst $ findMostLikely dict

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
