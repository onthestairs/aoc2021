{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day21 (solution) where

import AOC (Parser, Solution (..), parseFile, parseInt)
import Control.Lens
import Data.Function.Memoize (memoFix3)
import Data.Generics.Labels
import Relude hiding (cycle)
import Text.Megaparsec (eof, sepBy1)
import Text.Megaparsec.Char (newline, string)

type Input = (Int, Int)

parseInts :: Parser Input
parseInts = do
  string "Player 1 starting position: "
  s1 <- parseInt
  newline
  string "Player 2 starting position: "
  s2 <- parseInt
  eof
  pure (s1, s2)

data Player = Player
  { position :: Int,
    score :: Int
  }
  deriving (Show, Generic)

data Game = Game
  { nextDice :: Int,
    numberOfRolls :: Int,
    nextTurnIsPlayer1 :: Bool,
    player1 :: Player,
    player2 :: Player
  }
  deriving (Show, Generic)

cycle :: Int -> Int -> Int -> Int
cycle n d t = ((n -1 + d) `mod` t) + 1

getDice :: State Game Int
getDice = do
  d <- use #nextDice
  modifying #nextDice (\d -> cycle d 1 100)
  #numberOfRolls += 1
  pure d

turn :: State Game Int
turn = do
  isPlayer1Turn <- use #nextTurnIsPlayer1
  modifying #nextTurnIsPlayer1 not
  let playerId = if isPlayer1Turn then 1 else 2
  dice <- replicateM 3 getDice
  let totalDice = sum (dice)
  case playerId of
    1 -> do
      currentPosition <- use (#player1 . #position)
      let nextPosition = cycle currentPosition totalDice 10
      assign (#player1 . #position) nextPosition
      (#player1 . #score) += nextPosition
      score <- use (#player1 . #score)
      if score >= 1000 then pure 1 else turn
    2 -> do
      currentPosition <- use (#player2 . #position)
      let nextPosition = cycle currentPosition totalDice 10
      assign (#player2 . #position) nextPosition
      (#player2 . #score) += nextPosition
      score <- use (#player2 . #score)
      if score >= 1000 then pure 2 else turn

solve1 (p1, p2) = numberOfRolls * losingScore
  where
    initialState =
      Game
        { nextDice = 1,
          numberOfRolls = 0,
          nextTurnIsPlayer1 = True,
          player1 =
            Player
              { position = p1,
                score = 0
              },
          player2 =
            Player
              { position = p2,
                score = 0
              }
        }
    (winner, finalState) = runState turn initialState
    numberOfRolls = view #numberOfRolls finalState
    losingScore = if winner == 1 then view (#player2 . #score) finalState else view (#player1 . #score) finalState

data Wins = Wins Int Int deriving (Show)

instance Semigroup Wins where
  (Wins s11 s21) <> (Wins s12 s22) = Wins (s11 + s12) (s21 + s22)

instance Monoid Wins where
  mempty = Wins 0 0

diracDiceOutcomes = [1, 2, 3]

wins :: ((Int, Int) -> (Int, Int) -> Bool -> Wins) -> (Int, Int) -> (Int, Int) -> Bool -> Wins
wins wins' (p1, s1) (p2, s2) True = if (s2 >= 21) then Wins 0 1 else ws
  where
    ws = mconcat [let p = cycle p1 d 10 in wins' (p, s1 + p) (p2, s2) False | d <- diracDiceOutcomes]
wins wins' (p1, s1) (p2, s2) False = if (s1 >= 21) then Wins 1 0 else ws
  where
    ws = mconcat [let p = cycle p2 d 10 in wins' (p1, s1) (p, s2 + p) False | d <- diracDiceOutcomes]

winsMemo = memoFix3 wins

solve2 (p1, p2) = winsMemo (p1, 0) (p2, 0) True

solution =
  Solution
    { _parse = parseFile "21.txt" parseInts,
      _solve1 = solve1,
      _solve2 = solve2
    }
