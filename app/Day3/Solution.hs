{-# LANGUAGE OverloadedStrings #-}

module Day3.Solution where

import Data.Text (Text)
import Text.Megaparsec (runParser, skipManyTill, try)
import Text.Megaparsec.Char (char, newline, printChar, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Utils

pInstruction :: Parser (Int, Int)
pInstruction = string "mul(" *> ((,) <$> decimal <*> (char ',' *> decimal)) <* char ')'

pLine :: Parser [(Int, Int)]
pLine =
  many' (skipManyTill printChar (try pInstruction))

pLines :: Parser [(Int, Int)]
pLines = concat <$> pLine `sepBy'` newline

solve :: ([(Int, Int)] -> Int) -> Text -> Int
solve f =
  either
    (const 0)
    f
    . runParser pLines ""

solve1 :: Text -> Int
solve1 = solve $ sum . fmap (uncurry (*))

solve2 :: Text -> Int
solve2 = const 0
