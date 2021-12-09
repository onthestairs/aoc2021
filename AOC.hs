{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AOC (Solution (..), SeparateParseSolution (..), GenericSolution (..), Parser, parseFile, parseInt, parseDigit, parseInt64, parseInteger, parseSignedInt) where

import Control.Lens
import Relude
import Text.Megaparsec (Parsec, runParser)
import Text.Megaparsec.Char (char, digitChar)
import qualified Text.Megaparsec.Char.Lexer as L

data Solution a b c = Solution
  { _parse :: IO (Maybe a),
    _solve1 :: a -> b,
    _solve2 :: a -> c
  }
  deriving (Generic)

data SeparateParseSolution a b c d = SeparateParseSolution
  { _parse1 :: IO (Maybe a),
    _solveWith1 :: a -> b,
    _parse2 :: IO (Maybe c),
    _solveWith2 :: c -> d
  }
  deriving (Generic)

data GenericSolution where
  SimpleSolution :: (Show a, Show b, Show c) => Solution a b c -> GenericSolution
  TwoParseSolution :: (Show a, Show b, Show c, Show d) => SeparateParseSolution a b c d -> GenericSolution

type Path = String

type Parser = Parsec Void Text

parseFile :: Path -> Parser a -> IO (Maybe a)
parseFile path parse = do
  fileContents <- readFileText ("./data/" <> path)
  let result = runParser parse path fileContents
  pure $ rightToMaybe result

readDigit '0' = Just 0
readDigit '1' = Just 1
readDigit '2' = Just 2
readDigit '3' = Just 3
readDigit '4' = Just 4
readDigit '5' = Just 5
readDigit '6' = Just 6
readDigit '7' = Just 7
readDigit '8' = Just 8
readDigit '9' = Just 9

parseDigit :: Parser Int
parseDigit = do
  c <- digitChar
  case readDigit c of
    Just n -> pure n
    Nothing -> fail "not a digit"

parseInt :: Parser Int
parseInt = L.decimal

parseInt64 :: Parser Int64
parseInt64 = L.decimal

parseInteger :: Parser Integer
parseInteger = L.decimal

-- parseSignedInt :: Parser Int
parseSignedInt = do
  sign <- char '+' <|> char '-'
  n <- parseInt
  let coefficient = if sign == '+' then 1 else -1
  pure $ coefficient * n