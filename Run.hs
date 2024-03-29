{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run where

import AOC
import Control.Lens
import Data.Generics.Labels
import qualified Data.Map.Strict as Map
import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07
import Day08
import Day09
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16
import Day17
import Day18
import Day20
import Day21
import Day22
import Relude

solutions =
  Map.fromList
    [ (1, SimpleSolution Day01.solution),
      (2, SimpleSolution Day02.solution),
      (3, SimpleSolution Day03.solution),
      (4, SimpleSolution Day04.solution),
      (5, SimpleSolution Day05.solution),
      (6, SimpleSolution Day06.solution),
      (7, SimpleSolution Day07.solution),
      (8, SimpleSolution Day08.solution),
      (9, SimpleSolution Day09.solution),
      (10, SimpleSolution Day10.solution),
      (11, SimpleSolution Day11.solution),
      (12, SimpleSolution Day12.solution),
      (13, SimpleSolution Day13.solution),
      (14, SimpleSolution Day14.solution),
      (15, SimpleSolution Day15.solution),
      (16, SimpleSolution Day16.solution),
      (17, SimpleSolution Day17.solution),
      (18, SimpleSolution Day18.solution),
      (20, SimpleSolution Day20.solution),
      (21, SimpleSolution Day21.solution),
      (22, SimpleSolution Day22.solution)
    ]

data Part = Part1 | Part2 | Both deriving (Eq)

peekInput :: Int -> IO ()
peekInput day = do
  let maybeSolution = Map.lookup day solutions
  case maybeSolution of
    Nothing -> putStrLn "No solution"
    Just (SimpleSolution solution) -> do
      parsed <- view #_parse solution
      print parsed
    Just (TwoParseSolution solution) -> do
      parsed1 <- view #_parse1 solution
      putStrLn "Part 1"
      parsed2 <- view #_parse2 solution
      putStrLn "Part 2"
      print parsed2

showSolution :: GenericSolution -> Part -> IO ()
showSolution (SimpleSolution solution) part = do
  maybeParsed <- view #_parse solution
  case maybeParsed of
    Nothing -> putStrLn "Couldn't parse"
    Just parsed -> do
      let part1Result = view #_solve1 solution $ parsed
      let part2Result = view #_solve2 solution $ parsed
      case part of
        Part1 -> print part1Result
        Part2 -> print part2Result
        Both -> do
          print part1Result
          print part2Result
showSolution (TwoParseSolution solution) part = do
  when (part == Part1 || part == Both) $ do
    maybeParsed1 <- view #_parse1 solution
    case maybeParsed1 of
      Nothing -> putStrLn "Couldn't parse part i"
      Just parsed1 -> do
        print $ (view #_solveWith1 solution) parsed1
  when (part == Part2 || part == Both) $ do
    maybeParsed2 <- view #_parse2 solution
    case maybeParsed2 of
      Nothing -> putStrLn "Couldn't parse part ii"
      Just parsed2 -> do
        print $ (view #_solveWith2 solution) parsed2

solve :: Int -> Part -> IO ()
solve day part = do
  let maybeSolution = Map.lookup day solutions
  case maybeSolution of
    Nothing -> putStrLn "No solution"
    Just solution -> showSolution solution part

solveAll :: IO ()
solveAll = do
  let solutionsList = Map.toList solutions
  for_ solutionsList \(day, solution) -> do
    putStrLn $ "Day " <> show day
    putStrLn "------"
    showSolution solution Both
    putStrLn ""