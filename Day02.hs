{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day02 (solution) where

import AOC (Parser, Solution (..), parseFile, parseInt)
import Control.Lens
import Relude hiding (Down)
import Text.Megaparsec (eof, sepBy1)
import Text.Megaparsec.Char (char, newline, string)

type Input = [Instruction]

data Direction = Up | Down | Forward deriving (Show)

data Instruction = Instruction Direction Int deriving (Show)

parseInstructions :: Parser Input
parseInstructions = sepBy1 parseInstruction newline <* eof

parseDirection = (string "up" $> Up) <|> (string "down" $> Down) <|> (string "forward" $> Forward)

parseInstruction = do
  d <- parseDirection
  char ' '
  n <- parseInt
  pure $ Instruction d n

step :: Instruction -> State (Int, Int) ()
step (Instruction Up n) = _1 -= n
step (Instruction Down n) = _1 += n
step (Instruction Forward n) = _2 += n

solve1 is = (\(d, h) -> d * h) $ execState program (0, 0)
  where
    program :: State (Int, Int) ()
    program = forM_ is step

step2 :: Instruction -> State (Int, Int, Int) ()
step2 (Instruction Up n) = _3 -= n
step2 (Instruction Down n) = _3 += n
step2 (Instruction Forward n) = do
  _2 += n
  a <- use _3
  _1 += (a * n)

solve2 is = (\(d, h, _) -> d * h) $ execState program (0, 0, 0)
  where
    program = forM_ is step2

solution =
  Solution
    { _parse = parseFile "02.txt" parseInstructions,
      _solve1 = solve1,
      _solve2 = solve2
    }
