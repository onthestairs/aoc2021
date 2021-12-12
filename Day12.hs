{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day12 (solution) where

import AOC (Parser, Solution (..), parseFile, parseInt)
import Data.Char (isLower)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Relude
import Text.Megaparsec (eof, oneOf, sepBy1)
import Text.Megaparsec.Char (char, letterChar, newline)

type Input = [(String, String)]

parseInput :: Parser Input
parseInput = sepBy1 parseEdge newline <* eof

parseVertex = some (oneOf (['a' .. 'z'] <> ['A' .. 'Z']))

parseEdge = do
  v1 <- parseVertex
  char '-'
  v2 <- parseVertex
  pure (v1, v2)

makeGraph es = foldl' f Map.empty (es <> map swap es)
  where
    f m (v1, v2) = Map.insertWith (<>) v1 (Set.singleton v2) m

edges g v = toList $ fromMaybe mempty $ Map.lookup v g

isSmall v = all isLower v

findPaths g = go "start" []
  where
    go "end" p = [("end" : p)]
    go v p = [p' | v' <- goodNexts, p' <- go v' (v : p)]
      where
        next = edges g v
        goodNexts = filter isGood next
        isGood v' = (not $ isSmall v') || (not $ v' `elem` p)

solve1 es = length $ findPaths (makeGraph es)

count xs x = length $ filter ((==) x) xs

findPaths2 g = go "start" ([], False)
  where
    go "end" (p, _) = [("end" : p)]
    go v (p, visitedSmallCaveTwice) = [p' | (v', n) <- goodNexts, p' <- go v' (nextP, visitedSmallCaveTwice || (isSmall v' && n == 1))]
      where
        nextP = v : p
        next = edges g v
        goodNexts = filter isGood (zip next (map numberOfVisits next))
        numberOfVisits v' = count nextP v'
        isGood (v', n) = (v' /= "start") && ((not $ isSmall v') || (n == 0) || (n == 1 && not visitedSmallCaveTwice))

solve2 es = length $ findPaths2 (makeGraph es)

solution =
  Solution
    { _parse = parseFile "12.txt" parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
