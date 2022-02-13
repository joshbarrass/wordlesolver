import Common

oneRound :: Int -> String -> IO [Result]
oneRound round correct = do
  putStr $ "Make a guess (" ++ show round ++ "/6):\n"
  guess <- getLine
  return (check correct guess)

displayResult :: [Result] -> IO ()
displayResult xs = do
  putStrLn $ showResult xs
  return ()

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

main = do
  dict <- loadDictionary "words.txt"
  target <- chooseWord dict
  playGame target 0
