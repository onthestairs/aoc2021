{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day09 (solution) where

import AOC (Parser, Solution (..), parseDigit, parseFile)
import qualified Data.Matrix as Matrix
import qualified Data.Set as Set
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

setPop :: Ord a => Set.Set a -> Maybe (Set.Set a, a)
setPop s = if Set.size s == 0 then Nothing else Just (Set.delete v s, v)
  where
    v = Set.elemAt 0 s

findBasin :: Matrix.Matrix Int -> (Int, Int) -> Set.Set (Int, Int)
findBasin m c = go (Set.empty) (Set.singleton c) (Set.empty)
  where
    go b q seen = case setPop q of
      Just (q', next) -> go (b <> Set.singleton next) (q' <> frontier) (seen <> Set.singleton next)
        where
          cs = neighbours m next
          basinCs = Set.fromList $ filter (\c' -> getAt m c' /= 9) cs
          frontier = Set.difference basinCs seen
      Nothing -> b

findBasins m = go non9Cs
  where
    cs = coords m
    non9Cs = Set.fromList $ filter (\c -> getAt m c /= 9) cs
    go q = case setPop q of
      Just (q', next) -> basin : (go (Set.difference q' basin))
        where
          basin = findBasin m next
      Nothing -> []

solve2 nss = product $ take 3 $ reverse $ sort $ map Set.size bs
  where
    m = Matrix.fromLists nss
    bs = findBasins m

solution =
  Solution
    { _parse = parseFile "09.txt" parseInts,
      _solve1 = solve1,
      _solve2 = solve2
    }
