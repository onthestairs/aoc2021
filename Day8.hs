{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day8 (solution) where

import AOC (IOSolution (..), Parser, parseFile, parseInt)
-- import Control.Monad.Watson (satisfying)
-- import Data.Holmes hiding (Input, satisfying)
import Data.Holmes hiding (Input)
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

count1478s :: [[Char]] -> Int
count1478s ds = length $ filter (\d -> let n = length d in n `elem` [2, 4, 3, 7]) ds

solve1 :: Input -> IO Int
solve1 is = pure $ sum $ map (count1478s . snd) is

data Signal = SA | SB | SC | SD | SE | SF | SG
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (Hashable)

charToSignal c = toEnum (ord c - 97)

possibleDigits' :: [Char] -> [[Char]]
possibleDigits' d = map snd $ filter (\(n, cs) -> length cs == length d) digitsMap

charToV vs c = fromMaybe (error ("no v for c:" <> show c <> show index)) (vs !!? index)
  where
    index = ord c - 97

charsToVs vs cs = map (charToV vs) cs

digitSegmentsMatch :: forall m. MonadCell m => [Prop m (Intersect Signal)] -> [Char] -> [Char] -> Prop m (Intersect Bool)
digitSegmentsMatch vs d digitSegments = and' [or' [(lift $ charToSignal c) .== (charToV vs c2) | c2 <- digitSegments] | c <- d]

constrainSignals :: forall m. MonadCell m => [Prop m (Intersect Signal)] -> [Char] -> Prop m (Intersect Bool)
constrainSignals vs d = or' [digitSegmentsMatch vs d possibleDigit | possibleDigit <- possibleDigits]
  where
    possibleDigits = possibleDigits' d

constraints :: forall m. MonadCell m => [[Char]] -> [Prop m (Intersect Signal)] -> Prop m (Intersect Bool)
constraints ds vs = (distinct vs) .&& (and' (map (\d -> constrainSignals vs d) ds))

solveMapping ds = do
  -- v[0] is what 'a' on the wire, is as a segment
  -- let vs :: Config Holmes (Intersect Signal)
  let vs = 7 `from` [SA .. SG]
  vs `satisfying` (constraints ds)

toLetter :: Signal -> Char
toLetter s = toEnum $ (+) 97 $ fromEnum s

extractLetter l = toLetter $ fromMaybe (error "bad solution") $ viaNonEmpty head (toList l)

makeMapping os = zip (map extractLetter os) ['a' .. 'g']

deduceMapping :: [[Char]] -> IO (Maybe [(Char, Char)])
deduceMapping ds = do
  solution <- solveMapping ds
  pure $ makeMapping <$> solution

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

deduceOutput :: ([[Char]], [[Char]]) -> IO (Maybe Int)
deduceOutput (is, os) = do
  mapping <- deduceMapping is
  let justMapping = fromMaybe (error "no solution") (mapping)
  let ns = mapM (\o -> decode justMapping o) os
  pure $ toInt <$> ns

-- solve2 ((is, os) : ns) = deduceOutput (is, os)

solve2 ns = do
  maybeDs <- mapM deduceOutput ns
  let ds = sequence maybeDs
  pure $ sum <$> ds

solution =
  IOSolution
    { _parseIO = parseFile "8.txt" parseInput,
      _solve1IO = solve1,
      _solve2IO = solve2
    }
