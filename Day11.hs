{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day11 (solution) where

import AOC (Parser, Solution (..), parseDigit, parseFile)
import Control.Lens (Lens', lens, over, set, view, _1, _2)
import qualified Data.Matrix as Matrix
-- import Data.Matrix.Lens (elemAt)
import qualified Data.Set as Set
import Relude hiding (fix)
import Text.Megaparsec (eof, sepBy1)
import Text.Megaparsec.Char (newline)

type Input = [[Int]]

parseInts :: Parser Input
parseInts = sepBy1 (some parseDigit) newline <* eof

getAt m c = (uncurry Matrix.getElem) c m

isInBounds m (row, col) = row >= 1 && col >= 1 && row <= (Matrix.nrows m) && col <= (Matrix.ncols m)

neighbours m (row, col) = filter (isInBounds m) [(row + drow, col + dcol) | drow <- [-1, 0, 1], dcol <- [-1, 0, 1], (drow, dcol) /= (0, 0)]

coords m = [(row, col) | row <- [1 .. Matrix.nrows m], col <- [1 .. Matrix.ncols m]]

fixM :: (Monad m, Eq a) => (a -> m a) -> a -> m a
fixM f x = do
  x' <- f x
  if x == x' then pure x else fixM f x'

elemAt :: (Int, Int) -> Lens' (Matrix.Matrix a) a
elemAt (i, j) = lens (Matrix.getElem i j) (\m x -> Matrix.setElem x (i, j) m)

propStep :: Matrix.Matrix (Int, Bool) -> State Int (Matrix.Matrix (Int, Bool))
propStep m = do
  let cs = filter (\c -> view (elemAt c . _1) m > 9 && view (elemAt c . _2) m == False) (coords m)
  modify (+ length cs)
  let m' = foldl' (\m c -> set (elemAt c . _2) True m) m cs
  let m'' = foldl' (\m c -> over (elemAt c . _1) (+ 1) m) m' (concatMap (neighbours m) cs)
  pure $ m''

prop m = fmap fst <$> fixM propStep (fmap (\n -> (n, False)) m)

step m = do
  let m' = (fmap (+ 1) m)
  m'' <- prop m'
  pure $ fmap (\n -> if n > 9 then 0 else n) m''

iterateM :: Monad m => Int -> (a -> m a) -> a -> m [a]
iterateM 0 f x = pure []
iterateM n f x = do
  x' <- f x
  next <- iterateM (n - 1) f x'
  pure $ (x : next)

solve1 nss = (flip execState) 0 $ do
  let m = Matrix.fromLists nss
  ms <- iterateM 100 step m
  pure ms

iterateUntilM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m [a]
iterateUntilM p f x = do
  x' <- f x
  if p x'
    then pure []
    else do
      next <- iterateUntilM p f x'
      pure $ x : next

allZero = all ((==) 0)

solve2 nss = (flip evalState) 0 $ do
  let m = Matrix.fromLists nss
  ms <- iterateUntilM (allZero) step m
  pure $ 1 + length ms

solution =
  Solution
    { _parse = parseFile "11.txt" parseInts,
      _solve1 = solve1,
      _solve2 = solve2
    }
