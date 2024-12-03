module Utils (Parser, choice, tryDefault, many', sepBy', atOdds, atEvens) where

import Control.Applicative
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec (Parsec, try)

type Parser = Parsec Void Text

choice :: (Alternative f, Foldable t, Functor t) => t a -> f a
choice = foldl (<|>) empty . (pure <$>)

tryDefault ::
  -- | Attempt this parser first.
  Parser a ->
  -- | Fallback parser.
  Parser b ->
  -- | Next step if successful.
  (a -> Parser b) ->
  -- | Combined parser.
  Parser b
tryDefault a b f = optional (try a) >>= maybe b f

many' :: Parser a -> Parser [a]
many' p = tryDefault p (return []) ((<$> many' p) . (:))

some' :: Parser a -> Parser [a]
some' p = (:) <$> p <*> many' p

sepBy' :: Parser a -> Parser sep -> Parser [a]
sepBy' p sep =
  tryDefault
    p
    (return [])
    ((<$> (many' (sep >> p) <|> return [])) . (:))

atOdds :: [a] -> [a]
atOdds [] = []
atOdds (x : xs) = x : atEvens xs

atEvens :: [a] -> [a]
atEvens [] = []
atEvens (x : xs) = atOdds xs