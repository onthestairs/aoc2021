{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day22 (solution) where

import AOC (Parser, Solution (..), parseFile, parseInt, parseSignedInt)
import qualified Data.Set as Set
import Relude
import Text.Megaparsec (eof, sepBy1)
import Text.Megaparsec.Char (newline, string)

data Cuboid = Cuboid (Int, Int) (Int, Int) (Int, Int) deriving (Show)

type Input = [(Bool, Cuboid)]

parseInts :: Parser Input
parseInts = sepBy1 parseCuboid newline <* eof

parseSignedInt' = parseSignedInt <|> parseInt

parseRange = do
  n1 <- parseSignedInt'
  string ".."
  n2 <- parseSignedInt'
  pure (n1, n2)

parseToggle = (string "on" $> True) <|> (string "off" $> False)

parseCuboid = do
  t <- parseToggle
  string " x="
  xs <- parseRange
  string ",y="
  ys <- parseRange
  string ",z="
  zs <- parseRange
  pure (t, Cuboid xs ys zs)

clamp x limit = if x < (- limit) then (- limit) else if x > limit then limit else x

interval n1 n2 limit = if n2 < - limit || n1 > limit then [] else [clamp n1 limit .. clamp n2 limit]

cuboidPoints (Cuboid (minX, maxX) (minY, maxY) (minZ, maxZ)) limit = Set.fromList [(x, y, z) | x <- interval minX maxX 50, y <- interval minY maxY 50, z <- interval minZ maxZ 50]
  where

toggleCuboid True c s = Set.union s (cuboidPoints c 50)
toggleCuboid False c s = Set.difference s (cuboidPoints c 50)

solve1 cs = Set.size $ foldl' (\s (on, c) -> toggleCuboid on c s) Set.empty cs

solve2 ns = 2

solution =
  Solution
    { _parse = parseFile "22.txt" parseInts,
      _solve1 = solve1,
      _solve2 = solve2
    }
