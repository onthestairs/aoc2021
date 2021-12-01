{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day1 (solution) where

import AOC (Parser, Solution (..), parseFile, parseInt)
import Relude
import Text.Megaparsec (eof, sepBy1)
import Text.Megaparsec.Char (newline)

type Input = [Int]

parseInts :: Parser Input
parseInts = sepBy1 parseInt newline <* eof

tail' (x : xs) = xs
tail' [] = []

solve1 ns = length $ filter (\(n1, n2) -> n2 > n1) pairs
  where
    pairs = zip ns (tail' ns)

windows3 ns = map (\(n1, n2, n3) -> n1 + n2 + n3) $ zip3 ns (tail' ns) (tail' (tail' ns))

solve2 ns = length $ filter (\(n1, n2) -> n2 > n1) pairs
  where
    ms = windows3 ns
    pairs = zip ms (tail' ms)

solution =
  Solution
    { _parse = parseFile "1.txt" parseInts,
      _solve1 = solve1,
      _solve2 = solve2
    }
