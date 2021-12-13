{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day13 (solution) where

import AOC (Parser, Solution (..), parseFile, parseInt, sepByNewline)
import qualified Data.Set as Set
import Relude
import Relude.Extra.Foldable1
import Text.Megaparsec (eof, sepBy1)
import Text.Megaparsec.Char (char, newline, string)
import qualified Text.Show

data Direction = Y | X deriving (Show)

data Fold = Fold Direction Int deriving (Show)

type Input = ([(Int, Int)], [Fold])

parseInts :: Parser Input
parseInts = do
  ds <- parseDots
  newline
  newline
  fs <- parseFolds
  eof
  pure (ds, fs)

parseDot = do
  x <- parseInt
  char ','
  y <- parseInt
  pure (x, y)

parseDots = sepByNewline parseDot

parseFold = do
  string "fold along "
  axis <- (char 'x' *> pure X) <|> (char 'y' *> pure Y)
  char '='
  n <- parseInt
  pure (Fold axis n)

parseFolds = sepBy1 parseFold newline

data Paper = Paper {unpaper :: (Set.Set (Int, Int))}

getBounds :: Set.Set (Int, Int) -> Maybe ((Int, Int), (Int, Int))
getBounds paper = do
  x0 <- viaNonEmpty minimum1 (map fst (Set.toList paper))
  x1 <- viaNonEmpty maximum1 (map fst (Set.toList paper))
  y0 <- viaNonEmpty minimum1 (map snd (Set.toList paper))
  y1 <- viaNonEmpty maximum1 (map snd (Set.toList paper))
  pure $ ((x0, y0), (x1, y1))

instance Show Paper where
  show (Paper paper) = case getBounds paper of
    Just ((x0, y0), (x1, y1)) -> intercalate "\n" rows
      where
        makeRow y = map (\x -> if Set.member (x, y) paper then '#' else '.') [x0 .. x1]
        rows = map makeRow [y0 .. y1]
    Nothing -> "No size to paper"

makePaper ds = Paper $ Set.fromList ds

foldPaper (Paper paper) fold = Paper $ Set.map (f fold) paper
  where
    f (Fold X n) (x, y) = if x <= n then (x, y) else (n - (x - n), y)
    f (Fold Y n) (x, y) = if y <= n then (x, y) else (x, n - (y - n))

solve1 (ds, (f : fs)) = Set.size $ unpaper $ foldPaper paper f
  where
    paper = makePaper ds

solve2 (ds, fs) = finalPaper
  where
    paper = makePaper ds
    finalPaper = foldl' foldPaper paper fs

solution =
  Solution
    { _parse = parseFile "13.txt" parseInts,
      _solve1 = solve1,
      _solve2 = solve2
    }
