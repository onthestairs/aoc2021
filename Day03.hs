{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day03 (solution) where

import AOC (Parser, Solution (..), parseFile, parseInt)
import Relude
import Text.Megaparsec (eof, sepBy1)
import Text.Megaparsec.Char (char, newline)

type Input = [[Int]]

parseInts :: Parser Input
parseInts = sepBy1 parseBinary newline <* eof

parseBit = (char '0' $> 0) <|> (char '1' $> 1)

parseBinary = many parseBit

winner bs = if length zeroes > length ones then 0 else 1
  where
    zeroes = filter ((==) 0) bs
    ones = filter ((==) 1) bs

toDec bs = go (reverse bs) 0
  where
    go (b : bs') n = (b * 2 ^ n) + (go bs' (n + 1))
    go [] _ = 0

invertBit 0 = 1
invertBit 1 = 0

invert bs = map invertBit bs

solve1 ns = γ * ε
  where
    cs = transpose ns
    winners = map winner cs
    γ = toDec winners
    ε = toDec (invert winners)

unsafeAt xs i = fromMaybe (error "bad index") $ xs !!? i

indexNotEqual :: Eq a => Int -> a -> [a] -> Bool
indexNotEqual i v xs = v' /= v
  where
    v' = unsafeAt xs i

diagnose :: ([[Int]] -> Int -> Int) -> [[Int]] -> [Int]
diagnose f ns = go ns 0
  where
    go [n] _ = n
    go ns i = go ns' (i + 1)
      where
        v = f ns i
        ns' = filter (indexNotEqual i v) ns

maxInPlace :: [[Int]] -> Int -> Int
maxInPlace ns i = if length zeroes > length ones then 0 else 1
  where
    bs = map (\bs -> unsafeAt bs i) ns
    zeroes = filter ((==) 0) bs
    ones = filter ((==) 1) bs

minInPlace ns i = invertBit $ maxInPlace ns i

ogr ns = diagnose maxInPlace ns

csr ns = diagnose minInPlace ns

solve2 ns = toDec (ogr ns) * toDec (csr ns)

solution =
  Solution
    { _parse = parseFile "03.txt" parseInts,
      _solve1 = solve1,
      _solve2 = solve2
    }
