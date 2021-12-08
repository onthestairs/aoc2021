{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day8 (solution) where

import AOC (Parser, Solution (..), parseFile, parseInt)
import Control.Monad.Watson (Watson, satisfying)
import Data.Holmes hiding (Input, satisfying)
import Relude hiding (lift, some)
import Text.Megaparsec (anySingle, eof, lookAhead, oneOf, sepBy1, some, try)
import Text.Megaparsec.Char (char, newline, string)

type Input = [([[Char]], [[Char]])]

parseInput :: Parser Input
parseInput = sepBy1 parseLine newline <* eof

parseDigit = some (oneOf ['a' .. 'g'])

parseDigitBlock = do
  x <- parseDigit
  xs <- many $
    try $ do
      char ' '
      c <- lookAhead anySingle
      when (c == '|') (fail "end of sequence")
      parseDigit
  pure $ x : xs

parseLine = do
  c1 <- parseDigitBlock
  string " | "
  c2 <- parseDigitBlock
  pure (c1, c2)

digitsMap = [(0, "abcefg"), (1, "cf"), (2, "acdeg"), (3, "acdfg"), (4, "bcdf"), (5, "abdfg"), (6, "abdefg"), (7, "acf"), (8, "abcdefg"), (9, "abcdfg")]

count1478s ds = length $ filter (\d -> let n = length d in n `elem` [2, 4, 3, 7]) ds

solve1 :: Input -> Int
solve1 is = sum $ map (count1478s . snd) is

data Signal = SA | SB | SC | SD | SE | SF | SG
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable)

charToSignal c = toEnum (ord c - 97)

possibleDigits :: [Char] -> [[Char]]
possibleDigits d = map snd $ filter (\(n, cs) -> length cs == length d) digitsMap

charToV vs c = fromMaybe (error "no v for c") (vs !!? index)
  where
    index = ord c - 97

charsToVs vs cs = map (charToV vs) cs

digitSegmentsMatch vs d digitSegments = and' [or' [(lift $ charToSignal c) .== (charToV vs c2) | c2 <- digitSegments] | c <- d]

constrainSignals vs d = or' [digitSegmentsMatch vs d possibleDigit | possibleDigit <- possibleDigits d]

constraints ds vs = (distinct vs) .&& (and' (map (\d -> constrainSignals vs d) ds))

solveMapping :: [[Char]] -> Maybe [Intersect Signal]
solveMapping ds = do
  -- v[0] is the segement shown for 'a' on the wire
  -- vs :: Config Holmes (Intersect Signal)
  let vs = 7 `from` [SA .. SG] :: forall h. Config (Watson h) (Intersect Signal)
  vs `satisfying` (constraints ds)

toLetter :: Signal -> Char
toLetter s = toEnum $ (+) 97 $ fromEnum s

extractLetter l = toLetter $ fromMaybe (error "bad solution") $ viaNonEmpty head (toList l)

makeMapping :: [Intersect Signal] -> [(Char, Char)]
makeMapping os = zip (map extractLetter os) ['a' .. 'g']

deduceMapping :: [[Char]] -> Maybe [(Char, Char)]
deduceMapping ds = do
  solution <- solveMapping ds
  pure $ makeMapping solution

findSignal :: [(Char, Char)] -> Char -> Maybe Char
findSignal mapping c = snd <$> find ((==) c . fst) mapping

decode :: [(Char, Char)] -> [Char] -> Maybe Int
decode mapping o = do
  segments <- mapM (findSignal mapping) o
  digit <- find ((==) (sort segments) . snd) digitsMap
  pure $ fst digit

toInt :: [Int] -> Int
toInt ns = go (reverse ns) 0
  where
    go (n : ns') p = n * 10 ^ p + go ns' (p + 1)
    go [] _ = 0

deduceOutput :: ([[Char]], [[Char]]) -> Maybe Int
deduceOutput (is, os) = do
  mapping <- deduceMapping is
  ns <- mapM (\o -> decode mapping o) os
  pure $ toInt ns

solve2 :: Input -> Maybe Int
solve2 ns = do
  ds <- mapM deduceOutput ns
  pure $ sum ds

solution =
  Solution
    { _parse = parseFile "8.txt" parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
