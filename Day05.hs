{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day05 (solution) where

import AOC (Parser, Solution (..), parseFile, parseInt)
import qualified Data.Map as Map
import Relude hiding (getLine)
import Text.Megaparsec (eof, sepBy1)
import Text.Megaparsec.Char (char, newline, string)

type Input = [((Int, Int), (Int, Int))]

parseCoord = do
  x <- parseInt
  char ','
  y <- parseInt
  pure (x, y)

parseLine :: Parser ((Int, Int), (Int, Int))
parseLine = do
  c1 <- parseCoord
  string " -> "
  c2 <- parseCoord
  pure (c1, c2)

parseInput :: Parser Input
parseInput = sepBy1 parseLine newline <* eof

sign x1 x2 = if x1 < x2 then 1 else -1

r n1 n2 = if n1 >= n2 then [n2 .. n1] else [n1 .. n2]

getLineCs ((x1, y1), (x2, y2)) =
  if x1 == x2
    then [(x1, y) | y <- r y1 y2]
    else
      if y1 == y2
        then [(x, y1) | x <- r x1 x2]
        else
          if abs (x2 - x1) == abs (y2 - y1)
            then [(x1 + d * sign x1 x2, y1 + d * sign y1 y2) | d <- [0 .. (abs (x2 - x1))]]
            else error "invalid line"

placeLines ls = foldl' f Map.empty (foldMap getLineCs ls)
  where
    f m c = Map.insertWith (+) c 1 m

solve1 ls = Map.size $ Map.filter (>= 2) $ placeLines ls'
  where
    ls' = filter (\((x1, y1), (x2, y2)) -> x1 == x2 || y1 == y2) ls

solve2 ls = Map.size $ Map.filter (>= 2) $ placeLines ls

solution =
  Solution
    { _parse = parseFile "05.txt" parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
