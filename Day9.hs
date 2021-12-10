{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day9 (solution) where

import AOC (Parser, Solution (..), parseDigit, parseFile)
import qualified Data.Matrix as Matrix
import Relude
import Text.Megaparsec (eof, sepBy1)
import Text.Megaparsec.Char (newline)

type Input = [[Int]]

parseInts :: Parser Input
parseInts = sepBy1 (some parseDigit) newline <* eof

getAt m c = (uncurry Matrix.getElem) c m

isInBounds m (row, col) = row >= 1 && col >= 1 && row <= (Matrix.nrows m) && col <= (Matrix.ncols m)

neighbours m (row, col) = filter (isInBounds m) [(row -1, col), (row + 1, col), (row, col -1), (row, col + 1)]

isLocalMinimum m c = all (\c' -> v < getAt m c') (neighbours m c)
  where
    v = getAt m c

coords m = [(row, col) | row <- [1 .. Matrix.nrows m], col <- [1 .. Matrix.ncols m]]

findLocalMinima m = map (getAt m) $ filter (isLocalMinimum m) (coords m)

solve1 nss = sum $ map succ $ findLocalMinima m
  where
    m = Matrix.fromLists nss

solve2 nss = 5

solution =
  Solution
    { _parse = parseFile "9.txt" parseInts,
      _solve1 = solve1,
      _solve2 = solve2
    }
