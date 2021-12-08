{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AOC (Solution (..), IOSolution (..), SeparateParseSolution (..), GenericSolution (..), Parser, parseFile, parseInt, parseInt64, parseInteger, parseSignedInt) where

import Control.Lens
import Relude
import Text.Megaparsec (Parsec, runParser)
import Text.Megaparsec.Char (char)
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

data IOSolution a b c = IOSolution
  { _parseIO :: IO (Maybe a),
    _solve1IO :: a -> IO b,
    _solve2IO :: a -> IO c
  }
  deriving (Generic)

data GenericSolution where
  SimpleSolution :: (Show a, Show b, Show c) => Solution a b c -> GenericSolution
  TwoParseSolution :: (Show a, Show b, Show c, Show d) => SeparateParseSolution a b c d -> GenericSolution
  IOSimpleSolution :: (Show a, Show b, Show c) => IOSolution a b c -> GenericSolution

type Path = String

type Parser = Parsec Void Text

parseFile :: Path -> Parser a -> IO (Maybe a)
parseFile path parse = do
  fileContents <- readFileText ("./data/" <> path)
  let result = runParser parse path fileContents
  pure $ rightToMaybe result

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