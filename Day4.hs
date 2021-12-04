{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day4 (solution) where

import AOC (Parser, Solution (..), parseFile, parseInt)
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

findWinner :: [[[Int]]] -> [Int] -> Maybe [[Int]]
findWinner cs ns = find (isWinner ns) cs

-- findFirstWinner ns cs = find (isJust . findWinner cs) (inits ns)
findFirstWinner ns cs = do
  (ns', c') <- find (isJust . snd) $ map (\ns' -> (ns', findWinner cs ns')) (inits ns)
  c <- c'
  pure (ns', c)

-- solve1 (ns, cs) = findFirstWinner ns cs

solve1 (ns, cs) = do
  (ns', c) <- findFirstWinner ns cs
  lastN <- viaNonEmpty last ns'
  let unmarkedMs = filter (\x -> not $ x `elem` ns') (concat c)
  pure $ (sum unmarkedMs) * lastN

-- pure $ unmarkedMs

solve2 ns = 1

solution =
  Solution
    { _parse = parseFile "4.txt" parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
