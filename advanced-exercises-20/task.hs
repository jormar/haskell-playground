import System.IO
import System.Exit
import Data.Char
import Data.List

main :: IO()
main = do
  hSetBuffering stdout NoBuffering

  System.IO.putStrLn "Specify the words to search:"
  words <- getWords
  -- putStr $ show words -- DEBUG

  System.IO.putStr "File to search: "
  fileName <- getLine
  fileContent <- readFile fileName
  -- fileContent <- readFile "lorem.txt" -- DEBUG

  (foundWords, notFoundWords) <- findWords words fileContent
  printWords foundWords " found"
  printWords notFoundWords " NOT found"

  System.Exit.exitSuccess

printWords :: [String] -> String -> IO ()
printWords [] sufix = return ()
printWords (x:xs) sufix = do
  putStrLn $ "\"" ++ x ++ "\"" ++ sufix
  printWords xs sufix
  

getWords :: IO [String]
getWords = do
  System.IO.putStr " > "
  word <- System.IO.getLine
  if word == "" then
    return []
  else do
    words <- getWords
    return (word : words)

-- | It returns a tuple with the Found and not found strings in other strings
findWords :: [String] -> String -> IO ([String], [String])
findWords words content = return $ getFoundTuples words content ([], [])
  where
    content' = map toLower content

    getFoundTuples :: [String] -> String -> ([String], [String]) -> ([String], [String])
    getFoundTuples [] _ (ys, ns) = (reverse ys, reverse ns)
    getFoundTuples (x:xs) content (ys, ns) =
      let x' = map toLower x in
        if x' `isInfixOf` content'
          then getFoundTuples xs content (x:ys, ns)
          else getFoundTuples xs content (ys, x:ns)
