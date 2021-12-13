{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day07 (solution) where

import AOC (Parser, Solution (..), parseFile, parseInt)
import Relude
import Relude.Extra.Foldable1
import Text.Megaparsec (eof, sepBy1)
import Text.Megaparsec.Char (char, newline)

type Input = [Int]

parseInts :: Parser Input
parseInts = sepBy1 parseInt (char ',') <* eof

range ns = do
  ns' <- nonEmpty ns
  pure (minimum1 ns', maximum1 ns')

distance1 n1 n2 = abs (n2 - n1)

sumDistances d ks n = sum [d k n | k <- ks]

solve distance ns = do
  (n1, n2) <- range ns
  let distances = map (sumDistances distance ns) [n1 .. n2]
  viaNonEmpty minimum1 distances

solve1 = solve distance1

distance2 n1 n2 = (k * (k + 1)) `div` 2
  where
    k = abs (n2 - n1)

solve2 = solve distance2

solution =
  Solution
    { _parse = parseFile "07.txt" parseInts,
      _solve1 = solve1,
      _solve2 = solve2
    }
