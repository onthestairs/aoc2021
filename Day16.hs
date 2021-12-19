{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day16 (solution) where

import AOC (Parser, Solution (..), parseFile, parseInt)
import Relude hiding (one)
import Relude.Extra.Foldable1
import Text.Megaparsec (eof, errorBundlePretty, parse, parseMaybe, sepBy1, try)
import Text.Megaparsec.Char (char, hexDigitChar, newline)
import Text.Megaparsec.Debug (dbg)

type Input = String

parseInput = some hexDigitChar

hexCharToBinary '0' = "0000"
hexCharToBinary '1' = "0001"
hexCharToBinary '2' = "0010"
hexCharToBinary '3' = "0011"
hexCharToBinary '4' = "0100"
hexCharToBinary '5' = "0101"
hexCharToBinary '6' = "0110"
hexCharToBinary '7' = "0111"
hexCharToBinary '8' = "1000"
hexCharToBinary '9' = "1001"
hexCharToBinary 'A' = "1010"
hexCharToBinary 'B' = "1011"
hexCharToBinary 'C' = "1100"
hexCharToBinary 'D' = "1101"
hexCharToBinary 'E' = "1110"
hexCharToBinary 'F' = "1111"

toBinary t = foldMap hexCharToBinary t

data Packet = Literal Int Int | Operator Int Int [Packet] deriving (Show)

zero :: Parser Int
zero = (char '0' $> 0)

one :: Parser Int
one = (char '1' $> 1)

parseBinDigit :: Parser Int
parseBinDigit = zero <|> one

parseBinDigitChar = char '0' <|> char '1'

binToInt :: [Int] -> Int
binToInt bs = go (reverse bs) 0
  where
    go (b : bs') n = (b * 2 ^ n) + (go bs' (n + 1))
    go [] _ = 0

parseBinInt n = do
  ds <- replicateM n parseBinDigit
  pure (binToInt ds)

parsePacket' = do
  p <- parsePacket
  optional $ some zero
  pure p

parsePacket = do
  version <- parseBinInt 3
  typeId <- parseBinInt 3
  r <- case typeId of
    4 -> parseLiteralPacket version
    n -> parseOperatorPacket version typeId
  pure r

parseLiteralPacket version = do
  let go = do
        b <- parseBinDigit
        bs <- replicateM 4 parseBinDigit
        case b of
          0 -> pure bs
          1 -> do
            next <- go
            pure $ bs <> next
  bs <- go
  let n = binToInt (bs)
  pure $ Literal version n

parseOperatorPacket version typeId = do
  lengthTypeId <- parseBinDigit
  case lengthTypeId of
    0 -> do
      l <- parseBinInt 15
      subPacketsBits <- replicateM l parseBinDigitChar
      ps <- case parseMaybe (some parsePacket) (toText subPacketsBits) of
        Just ps' -> pure ps'
        Nothing -> fail "incorrect number of subpackets"
      pure $ Operator version typeId ps
    1 -> do
      numberOfSubPackets <- parseBinInt 11
      ps <- replicateM (numberOfSubPackets) parsePacket
      pure $ Operator version typeId ps

sumPacketVersions (Literal v _) = v
sumPacketVersions (Operator v _ ps) = v + sum (map sumPacketVersions ps)

solve1 t = sumPacketVersions <$> parseMaybe parsePacket' binaryStr
  where
    binaryStr = toBinary t

eval (Literal _ n) = n
eval (Operator _ 0 ps) = sum (map eval ps)
eval (Operator _ 1 ps) = product (map eval ps)
eval (Operator _ 2 ps) = fromMaybe (error "min of nothing") $ viaNonEmpty minimum1 (map eval ps)
eval (Operator _ 3 ps) = fromMaybe (error "min of nothing") $ viaNonEmpty maximum1 (map eval ps)
eval (Operator _ 5 [p1, p2]) = if eval p1 > eval p2 then 1 else 0
eval (Operator _ 6 [p1, p2]) = if eval p1 < eval p2 then 1 else 0
eval (Operator _ 7 [p1, p2]) = if eval p1 == eval p2 then 1 else 0

solve2 t = eval <$> parseMaybe parsePacket' binaryStr
  where
    binaryStr = toBinary t

solution =
  Solution
    { _parse = parseFile "16.txt" parseInput,
      _solve1 = solve1,
      _solve2 = solve2
    }
