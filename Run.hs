{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run where

import AOC
import Control.Lens
import Data.Generics.Labels
import qualified Data.Map.Strict as Map
import Day1
-- import Day10
-- import Day11
-- import Day12
-- import Day13
-- import Day14
-- import Day15
-- import Day16
-- import Day17
-- import Day18
import Day2
import Day3
import Day4
import Day5
-- import Day6
-- import Day7
-- import Day8
-- import Day9
import Relude

solutions =
  Map.fromList
    [ (1, SimpleSolution Day1.solution),
      (2, SimpleSolution Day2.solution),
      (3, SimpleSolution Day3.solution),
      (4, SimpleSolution Day4.solution),
      (5, SimpleSolution Day5.solution)
      -- (6, SimpleSolution Day6.solution),
      -- (7, SimpleSolution Day7.solution),
      -- (8, SimpleSolution Day8.solution),
      -- (9, SimpleSolution Day9.solution),
      -- (10, SimpleSolution Day10.solution),
      -- (11, SimpleSolution Day11.solution),
      -- (12, SimpleSolution Day12.solution),
      -- (13, SimpleSolution Day13.solution),
      -- (14, SimpleSolution Day14.solution),
      -- (15, SimpleSolution Day15.solution),
      -- (16, SimpleSolution Day16.solution),
      -- (17, SimpleSolution Day17.solution),
      -- (18, TwoParseSolution Day18.solution)
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
      -- print parsed1
      parsed2 <- view #_parse2 solution
      putStrLn "Part 2"
      print parsed2

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