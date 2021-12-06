{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day6 (solution) where

import AOC (Parser, Solution (..), parseFile, parseInt)
import qualified Data.MultiSet as MultiSet
import Relude
import Text.Megaparsec (eof, sepBy1)
import Text.Megaparsec.Char (char, newline)

type Input = [Int]

parseInts :: Parser Input
parseInts = sepBy1 parseInt (char ',') <* eof

step s = MultiSet.concatMap (\n -> if n == 0 then [6, 8] else [n -1]) s

fish n = fmap MultiSet.size . viaNonEmpty head . drop n . iterate step . MultiSet.fromList

solve1 = fish 80

solve2 = fish 256

solution =
  Solution
    { _parse = parseFile "6.txt" parseInts,
      _solve1 = solve1,
      _solve2 = solve2
    }
