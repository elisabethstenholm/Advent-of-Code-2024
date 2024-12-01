module Main where

import qualified Data.Text as Text
import Day1.Solution (solve1, solve2)

main :: IO ()
main = do
  exData <- Text.pack <$> readFile "app/Day1/ex-input.txt"
  actualData <- Text.pack <$> readFile "app/Day1/input.txt"
  putStrLn "Part 1:"
  putStrLn $ "\tExample: " ++ show (solve1 exData)
  putStrLn $ "\tActual: " ++ show (solve1 actualData)
  putStrLn "Part 2:"
  putStrLn $ "\tExample: " ++ show (solve2 exData)
  putStrLn $ "\tActual: " ++ show (solve2 actualData)
