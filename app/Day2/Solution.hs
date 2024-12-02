module Day2.Solution where

import Data.List (subsequences)
import Data.Text (Text)
import Text.Megaparsec (runParser)
import Text.Megaparsec.Char (hspace1, newline)
import Text.Megaparsec.Char.Lexer (decimal)
import Utils

pLine :: Parser [Int]
pLine = decimal `sepBy'` hspace1

pLines :: Parser [[Int]]
pLines = pLine `sepBy'` newline

solve :: ([[Int]] -> Int) -> Text -> Int
solve f =
  either
    (const 0)
    f
    . runParser pLines ""

data LevelDiff = UnsafeDiff | SafeIncrease | SafeDecrease
  deriving (Eq, Show)

diff :: Int -> Int -> LevelDiff
diff prev current
  | prev - current `elem` [1, 2, 3] = SafeDecrease
  | current - prev `elem` [1, 2, 3] = SafeIncrease
  | otherwise = UnsafeDiff

diffs :: [Int] -> [LevelDiff]
diffs l =
  let pairs = zip l (tail l)
   in uncurry diff <$> pairs

type Safe = Bool

safe1 :: [Int] -> Safe
safe1 l
  | all (== SafeIncrease) (diffs l) = True
  | all (== SafeDecrease) (diffs l) = True
  | otherwise = False

solve1 :: Text -> Int
solve1 = solve $ length . filter id . fmap safe1

removeOneElement :: [a] -> [[a]]
removeOneElement l =
  let n = length l - 1
   in filter ((== n) . length) $ subsequences l

safe2 :: [Int] -> Safe
safe2 = any safe1 . removeOneElement

solve2 :: Text -> Int
solve2 = solve $ length . filter id . fmap (\l -> safe1 l || safe2 l)
