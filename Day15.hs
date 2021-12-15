{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day15 (solution) where

import AOC (Parser, Solution (..), parseDigit, parseFile)
import qualified Data.Graph.Inductive.Graph as Graph
import qualified Data.Graph.Inductive.PatriciaTree as PatriciaTree
import Data.Graph.Inductive.Query.SP (spLength)
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

coords m = [(row, col) | row <- [1 .. Matrix.nrows m], col <- [1 .. Matrix.ncols m]]

mapJoin f xs = zip xs (map f xs)

coordToInt m (row, col) = row * (Matrix.ncols m) + col

makeEdges m = foldMap (edges) (coords m)
  where
    edges c = [(c, c', getAt m c') | c' <- neighbours m c]

makeIntEdges m = map (\(c1, c2, n) -> (coordToInt m c1, coordToInt m c2, n)) (makeEdges m)

makeNodes m = [(coordToInt m c, c) | c <- coords m]

makeGraph :: [(Int, (Int, Int))] -> [(Int, Int, Int)] -> PatriciaTree.Gr (Int, Int) Int
makeGraph ns es = Graph.mkGraph ns es

solve m = spLength start end g
  where
    es = makeIntEdges m
    ns = makeNodes m
    g = makeGraph ns es
    start = coordToInt m (1, 1)
    end = coordToInt m (Matrix.nrows m, Matrix.ncols m)

solve1 nss = solve m
  where
    m = Matrix.fromLists nss

addOneWrap 9 = 1
addOneWrap n = n + 1

tileWith f n m = foldl' f m ms
  where
    ms = drop 1 $ take n $ iterate (fmap addOneWrap) m

tile m = tileWith (Matrix.<->) 5 (tileWith (Matrix.<|>) 5 m)

solve2 nss = solve m'
  where
    m = Matrix.fromLists nss
    m' = tile m

solution =
  Solution
    { _parse = parseFile "15.txt" parseInts,
      _solve1 = solve1,
      _solve2 = solve2
    }
