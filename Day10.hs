{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day10 (solution) where

import AOC (Parser, Solution (..), parseDigit, parseFile)
import qualified Data.Stack as Stack
import Relude
import Text.Megaparsec (eof, oneOf, sepBy1)
import Text.Megaparsec.Char (newline)

type Input = [String]

parseInput :: Parser Input
parseInput = sepBy1 (some (oneOf ['{', '(', '[', '<', '>', ']', ')', '}'])) newline <* eof

data ParseResult = Incomplete String | InvalidChar Char deriving (Show)

neededChars s = case Stack.stackPop s of
  Just (s', '(') -> (')' : neededChars s')
  Just (s', '[') -> (']' : neededChars s')
  Just (s', '{') -> ('}' : neededChars s')
  Just (s', '<') -> ('>' : neededChars s')
  Nothing -> ""

parseLine cs = go s cs
  where
    s = Stack.stackNew
    go s [] = Incomplete (neededChars s)
    go s (c : cs) = case c of
      '{' -> go (Stack.stackPush s c) cs
      '(' -> go (Stack.stackPush s c) cs
      '[' -> go (Stack.stackPush s c) cs
      '<' -> go (Stack.stackPush s c) cs
      '}' -> case Stack.stackPop s of
        Just (s', '{') -> go s' cs
        Just (s', c') -> InvalidChar '}'
        Nothing -> error "closing tag for nothing"
      ')' -> case Stack.stackPop s of
        Just (s', '(') -> go s' cs
        Just (s', c') -> InvalidChar ')'
        Nothing -> error "closing tag for nothing"
      ']' -> case Stack.stackPop s of
        Just (s', '[') -> go s' cs
        Just (s', c') -> InvalidChar ']'
        Nothing -> error "closing tag for nothing"
      '>' -> case Stack.stackPop s of
        Just (s', '<') -> go s' cs
        Just (s', c') -> InvalidChar '>'
        Nothing -> error "closing tag for nothing"

score ')' = 3
score ']' = 57
score '}' = 1197
score '>' = 25137

lineScore1 cs = case parseLine cs of
  InvalidChar c -> score c
  _ -> 0

solve1 ls = getSum $ foldMap (Sum . lineScore1) ls

charScore2 ')' = 1
charScore2 ']' = 2
charScore2 '}' = 3
charScore2 '>' = 4

score2 cs = go cs 0
  where
    go [] n = n
    go (c : cs) n = go cs (n * 5 + charScore2 c)

lineScore2 cs = case parseLine cs of
  Incomplete cs' -> Just $ score2 cs'
  _ -> Nothing

median xs = viaNonEmpty head $ drop (length xs `div` 2) xs

solve2 ls = median $ sort $ mapMaybe lineScore2 ls

solution =
  Solution
    { _parse = parseFile "10.txt" parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
