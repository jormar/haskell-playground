import System.IO

main :: IO()
main = do
  hSetBuffering stdout NoBuffering
  System.IO.putStrLn "Bye."
