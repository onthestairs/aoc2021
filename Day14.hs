{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day14 (solution) where

import AOC (Parser, Solution (..), parseFile, parseInt)
import Data.Function.Memoize (memoFix2)
import qualified Data.Map as Map
import qualified Data.MultiSet as MultiSet
import Relude
import Relude.Extra.Foldable1
import Text.Megaparsec (eof, sepBy1)
import Text.Megaparsec.Char (newline, string, upperChar)

type Input = (String, [((Char, Char), Char)])

parseInput :: Parser Input
parseInput = do
  template <- parseTemplate
  newline
  newline
  rs <- sepBy1 parseRule newline
  eof
  pure (template, rs)

parseTemplate = some upperChar

parseRule = do
  c1 <- upperChar
  c2 <- upperChar
  string " -> "
  c3 <- upperChar
  pure ((c1, c2), c3)

tail' (x : xs) = xs
tail' [] = []

pairs xs = zip xs (tail' xs)

makeRules rs = Map.fromList rs

solvePair rs f (c0, c1) 0 = MultiSet.fromList [c0, c1]
solvePair rs f (c0, c1) n = MultiSet.delete c' $ f (c0, c') (n -1) <> f (c', c1) (n -1)
  where
    lookup pair = fromMaybe (error "no rule found") (Map.lookup pair rs)
    c' = lookup (c0, c1)

inner xs = drop 1 (reverse $ drop 1 $ reverse xs)

calculateCounts :: [Char] -> [((Char, Char), Char)] -> Int -> MultiSet.MultiSet Char
calculateCounts t rs n = finalCounts
  where
    rules = makeRules rs
    solvePair' = memoFix2 (solvePair rules)
    correction = MultiSet.fromList (inner t)
    counts = foldMap (\c -> solvePair' c n) (pairs t)
    finalCounts = MultiSet.difference counts correction

biggestMinusSmallest counts = do
  let occurences = map snd $ MultiSet.toOccurList counts
  n1 <- viaNonEmpty minimum1 occurences
  n2 <- viaNonEmpty maximum1 occurences
  pure (n2 - n1)

solve1 (t, rs) = biggestMinusSmallest $ calculateCounts t rs 10

solve2 (t, rs) = biggestMinusSmallest $ calculateCounts t rs 40

solution =
  Solution
    { _parse = parseFile "14.txt" parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
