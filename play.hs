import System.Random

data Result = Wrong | Position | Correct | Unknown deriving (Show, Eq)

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

oneRound :: Int -> String -> IO [Result]
oneRound round correct = do
  putStr $ "Make a guess (" ++ show round ++ "/6):\n"
  guess <- getLine
  return (check correct guess)

displayResult :: [Result] -> IO ()
displayResult [] = putStr "\n"
displayResult (Correct:xs) = do
  putStr "O"
  displayResult xs
displayResult (Position:xs) = do
  putStr "?"
  displayResult xs
displayResult (Wrong:xs) = do
  putStr "X"
  displayResult xs

playGame :: String -> Int -> IO ()
playGame correct 6 = putStr $ "Sorry, the correct answer was: " ++ correct ++ "\n"
playGame correct round = do
  roundResult <- oneRound (round+1) correct
  displayResult roundResult
  if all (==Correct) roundResult then
    (do
        return ()
    ) else (do
               putStr "\n"
               playGame correct $ round+1
           )

loadDictionary :: FilePath -> IO [String]
loadDictionary fp = do
  contents <- readFile fp
  return $ lines contents

chooseWord :: [String] -> IO String
chooseWord dict = do
  gen <- getStdGen
  let (i, _) = randomR (0, length dict - 1) gen
  return $ dict !! i

main = do
  dict <- loadDictionary "words.txt"
  target <- chooseWord dict
  playGame target 0
