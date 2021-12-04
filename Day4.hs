{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day4 (solution) where

import AOC (Parser, Solution (..), parseFile, parseInt)
import Data.List (partition)
import Relude
import Text.Megaparsec (anySingle, eof, lookAhead, sepBy1, try)
import Text.Megaparsec.Char (char, hspace, newline)

type Input = ([Int], [[[Int]]])

twoNewlines = do
  newline
  newline

sepByNewline :: Parser a -> Parser [a]
sepByNewline p = do
  x <- p
  xs <- many $
    try $ do
      newline
      c <- lookAhead anySingle
      when (c == '\n') (fail "double newline")
      p
  pure $ x : xs

parseInput :: Parser Input
parseInput = do
  ns <- parseNs
  twoNewlines
  cards <- sepBy1 parseCard twoNewlines
  eof
  pure (ns, cards)

parseNs = sepBy1 parseInt (char ',')

parseCard = do
  let parseLine = (optional hspace) *> sepBy1 parseInt hspace
  sepByNewline parseLine

isWinner :: [Int] -> [[Int]] -> Bool
isWinner ns c = isWinnerRows ns c || isWinnerCols ns c

isWinnerRows :: [Int] -> [[Int]] -> Bool
isWinnerRows ns c = any (all (\x -> x `elem` ns)) c

isWinnerCols :: [Int] -> [[Int]] -> Bool
isWinnerCols ns c = isWinnerRows ns (transpose c)

winners [] cs = []
winners _ [] = []
winners (ns : nss) cs = ws <> (winners nss (map snd nws))
  where
    (ws, nws) = partition (\(ns', c) -> isWinner ns' c) (map (\c -> (ns, c)) cs)

getScore ns cs selector = do
  (ns', c) <- viaNonEmpty selector $ winners (inits ns) cs
  lastN <- viaNonEmpty last ns'
  let unmarkedMs = filter (\x -> not $ x `elem` ns') (concat c)
  pure $ (sum unmarkedMs) * lastN

solve1 (ns, cs) = getScore ns cs head

solve2 (ns, cs) = getScore ns cs last

solution =
  Solution
    { _parse = parseFile "4.txt" parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
