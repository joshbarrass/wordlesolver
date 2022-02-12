module Common
  ( Result(..)
  , loadDictionary
  , chooseWord
  , check
  , removeFirst
  , isPossible
  , filterDict
  ) where

import System.Random

data Result = Wrong | Position | Correct | Unknown deriving (Show, Eq)

loadDictionary :: FilePath -> IO [String]
loadDictionary fp = do
  contents <- readFile fp
  return $ lines contents

chooseWord :: [String] -> IO String
chooseWord dict = do
  gen <- getStdGen
  let (i, _) = randomR (0, length dict - 1) gen
  return $ dict !! i

removeFirst :: (Eq a) => a -> [a] -> [a]
removeFirst _ [] = []
removeFirst x (y:xs)
  | y == x = xs
  | otherwise = y : removeFirst x xs

isCorrect :: Char -> Char -> (Char, Result)
isCorrect a b = if a == b then (b, Correct) else (b, Unknown)

checkLoop [] _ = []
checkLoop xs [] = [if x == Unknown then Wrong else x | (_,x) <- xs]
checkLoop ((_, Correct):xs) remaining = Correct : checkLoop xs remaining
checkLoop ((c, Unknown):xs) remaining = let
  isValid = c `elem` remaining
  toInsert = if isValid then Position else Wrong
  newRemaining = if isValid then removeFirst c remaining else remaining
  in toInsert : checkLoop xs newRemaining

check :: String -> String -> [Result]
check true guess = let
  exact = zipWith isCorrect true guess
  remaining = map fst $ filter (\x -> snd (snd x) /= Correct) $ zip true exact
  in checkLoop exact remaining

-- checks whether a word is compatible with a result
isPossible :: String -> [Result] -> String -> Bool
isPossible guess result true = check true guess == result

-- filter dictionary to possible answers only
filterDict :: String -> [Result] -> [String] -> [String]
filterDict guess result = filter (isPossible guess result) 
