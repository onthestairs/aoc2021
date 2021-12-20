{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day18 (solution) where

import AOC (Parser, Solution (..), parseFile, parseInt)
import Relude
import Relude.Extra.Foldable1 (foldl1', maximum1)
import Text.Megaparsec (eof, parseMaybe, sepBy1)
import Text.Megaparsec.Char (char, newline)
import qualified Text.Show

data Pair a = P (Pair a) (Pair a) | L a

instance Show a => Show (Pair a) where
  show (P l r) = "[" <> show l <> "," <> show r <> "]"
  show (L n) = show n

type Input = [Pair Int]

parseInput :: Parser Input
parseInput = sepBy1 parsePair newline <* eof

parseLiteral = L <$> parseInt

parseNested = do
  char '['
  x1 <- parsePair
  char ','
  x2 <- parsePair
  char ']'
  pure (P x1 x2)

parsePair = parseLiteral <|> parseNested

add p1 p2 = reduce $ P p1 p2

addToLeft n (L m) = L (m + n)
addToLeft n (P p1 p2) = P (addToLeft n p1) p2

addToRight n (L m) = L (m + n)
addToRight n (P p1 p2) = P p1 (addToRight n p2)

fst3 (a, b, c) = a

explode p = fst3 <$> go 0 p
  where
    go 4 (P (L n1) (L n2)) = Just (L 0, n1, n2)
    go n (P p1 p2) = case go (n + 1) p1 of
      Just (p1', n1, n2) -> Just (P p1' (addToLeft n2 p2), n1, 0)
      Nothing -> case go (n + 1) p2 of
        Just (p2', n1, n2) -> Just (P (addToRight n1 p1) p2', 0, n2)
        Nothing -> Nothing
    go d o = Nothing

split p = go p
  where
    go (P p1 p2) = case go p1 of
      Just p1' -> Just $ P p1' p2
      Nothing -> case go p2 of
        Just p2' -> Just $ P p1 p2'
        Nothing -> Nothing
    go (L n) =
      if n >= 10
        then (Just $ P (L $ n `div` 2) (L $ n `div` 2 + n `rem` 2))
        else Nothing

reduce p = case explode (p) of
  Just p' -> reduce p'
  Nothing -> case split p of
    Just p' -> reduce p'
    Nothing -> p

magnitude (P p1 p2) = (3 * magnitude p1) + (2 * magnitude p2)
magnitude (L n) = n

solve1 ns = magnitude <$> viaNonEmpty (foldl1' add) ns

solve2 ns = viaNonEmpty maximum1 ms
  where
    ms = [magnitude $ add p1 p2 | p1 <- ns, p2 <- ns]

solution =
  Solution
    { _parse = parseFile "18.txt" parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
