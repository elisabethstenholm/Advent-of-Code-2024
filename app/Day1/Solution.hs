{-# LANGUAGE TupleSections #-}

module Day1.Solution where

import Data.Bifunctor (bimap)
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Megaparsec (runParser)
import Text.Megaparsec.Char (hspace1, newline)
import Text.Megaparsec.Char.Lexer (decimal)
import Utils

pLine :: Parser (Int, Int)
pLine = (,) <$> decimal <*> (hspace1 *> decimal)

pLines :: Parser [(Int, Int)]
pLines = pLine `sepBy'` newline

solve :: (([Int], [Int]) -> Int) -> Text -> Int
solve f =
  either
    (const 0)
    (f . unzip)
    . runParser pLines ""

f1 :: ([Int], [Int]) -> Int
f1 = sum . uncurry (zipWith (\x y -> abs (x - y))) . bimap sort sort

solve1 :: Text -> Int
solve1 = solve f1

count :: [Int] -> Map Int Int
count = foldr (Map.alter f) (Map.empty)
  where
    f :: Maybe Int -> Maybe Int
    f Nothing = Just 1
    f (Just n) = Just (n + 1)

f2 :: ([Int], [Int]) -> Int
f2 (l1, l2) =
  let m1 = count l1
      m2 = count l2
   in Map.foldrWithKey (\k v acc -> acc + k * v * (fromMaybe 0 $ Map.lookup k m2)) 0 m1

solve2 :: Text -> Int
solve2 = solve f2
