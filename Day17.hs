{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day17 (solution) where

import AOC (Parser, Solution (..), parseFile, parseInt, parseSignedInt)
import Control.Monad.State.Lazy
import Relude hiding (State, evalState)
import Relude.Extra.Foldable1
import Text.Megaparsec (eof, sepBy1)
import Text.Megaparsec.Char (newline, string)
import Text.Megaparsec.Debug (dbg)

data Target = Target (Int, Int) (Int, Int) deriving (Show)

type Input = Target

parseInts :: Parser Input
parseInts = do
  string "target area: x="
  x1 <- parseSignedInt <|> parseInt
  string ".."
  x2 <- parseSignedInt <|> parseInt
  string ", y="
  y1 <- parseSignedInt <|> parseInt
  string ".."
  y2 <- parseSignedInt <|> parseInt
  eof
  pure $ Target (min x1 x2, min y1 y2) (max x1 x2, max y1 y2)

isIn (Target (minX, minY) (maxX, maxY)) (x, y) = x >= minX && x <= maxX && y >= minY && y <= maxY

nudge x = case compare x 0 of
  EQ -> 0
  LT -> x + 1
  GT -> x - 1

step :: (Int, Int) -> State (Int, Int) (Int, Int)
step (x, y) = do
  (dx, dy) <- get
  let x' = x + dx
  let y' = y + dy
  modify (\(dx, dy) -> (nudge dx, dy -1))
  pure (x', y')

iterateM f x = do
  x' <- f x
  next <- iterateM f x'
  pure $ (x : next)

path d = evalState (iterateM step (0, 0)) d

pastTarget (Target (minX, minY) (maxX, maxY)) (x, y) = x > maxX || y < minY

findPath t d = do
  let cs = takeWhile (not . pastTarget t) (path d)
  lastC <- viaNonEmpty last cs
  if isIn t lastC
    then pure cs
    else Nothing

findGoodPaths t@(Target (minX, minY) (maxX, maxY)) = catMaybes (map (findPath t) pairs)
  where
    maxPossibleDx = maxX
    maxPossibleDy = abs minY
    pairs = [(dx, dy) | dx <- [0 .. maxPossibleDx], dy <- [minY .. maxPossibleDy]]

solve1 t = viaNonEmpty maximum1 (concat $ map (map snd) goodPaths)
  where
    goodPaths = findGoodPaths t

solve2 t = length goodPaths
  where
    goodPaths = findGoodPaths t

solution =
  Solution
    { _parse = parseFile "17.txt" parseInts,
      _solve1 = solve1,
      _solve2 = solve2
    }
