{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Day20 (solution) where

import AOC (Parser, Solution (..), parseFile, parseInt)
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import Relude
import Relude.Extra.Foldable1 (maximum1, minimum1)
import Text.Megaparsec (eof, sepBy1)
import Text.Megaparsec.Char (char, newline)
import Text.Show

type Input = ([Colour], [[Colour]])

data Colour = Dark | Light deriving (Eq, Show)

parsePixel = (char '.' $> Dark) <|> (char '#' $> Light)

parseInts :: Parser Input
parseInts = do
  algo <- some parsePixel
  newline
  newline
  image <- sepBy1 (some parsePixel) newline
  eof
  pure (algo, image)

binToInt :: [Bool] -> Int
binToInt bs = go (reverse bs) 0
  where
    go (True : bs') n = (2 ^ n) + (go bs' (n + 1))
    go (False : bs') n = (go bs' (n + 1))
    go [] _ = 0

localGridDeltas = [(-1, -1), (0, -1), (1, -1), (-1, 0), (0, 0), (1, 0), (-1, 1), (0, 1), (1, 1)]

localGrid (x, y) = [(x + dx, y + dy) | (dx, dy) <- localGridDeltas]

isOn g c = Set.member c g

toSet bss = Set.fromList [(x, y) | (y, row) <- zip [0 ..] bss, (x, l) <- zip [0 ..] row, l == Light]

coords ((minX, minY), (maxX, maxY)) = [(x, y) | x <- [minX .. maxX], y <- [minY .. maxY]]

getBounds s = fromMaybe (error "nothing in grid") $ do
  let xs = map fst $ Set.toList s
  let ys = map snd $ Set.toList s
  minX <- viaNonEmpty minimum1 xs
  maxX <- viaNonEmpty maximum1 xs
  minY <- viaNonEmpty minimum1 ys
  maxY <- viaNonEmpty maximum1 ys
  pure ((minX, minY), (maxX, maxY))

getBoundingCoords s = (coords bs) <> toList border1 <> toList border2
  where
    bs@((minX, minY), (maxX, maxY)) = getBounds s
    border1 = makeBorder (minX -1, minY -1) (maxX + 1, maxY + 1)
    border2 = makeBorder (minX -2, minY -2) (maxX + 2, maxY + 2)

makeBorder (minX, minY) (maxX, maxY) = Set.fromList $ top <> right <> left <> bottom
  where
    top = [(x, minY) | x <- [minX .. maxX]]
    bottom = [(x, maxY) | x <- [minX .. maxX]]
    left = [(minX, y) | y <- [minY .. maxY]]
    right = [(maxX, y) | y <- [minY .. maxY]]

enlarge Dark s = s
enlarge Light s = s <> border1 <> border2 <> border3
  where
    ((minX, minY), (maxX, maxY)) = getBounds s
    border1 = makeBorder (minX -1, minY -1) (maxX + 1, maxY + 1)
    border2 = makeBorder (minX -2, minY -2) (maxX + 2, maxY + 2)
    border3 = makeBorder (minX -3, minY -3) (maxX + 3, maxY + 3)

getIndex g c = binToInt $ map (isOn g) (localGrid c)

enhance algo g c = algo $ getIndex g c

step algo (borderColour, g) = (newBorderColour, s')
  where
    biggerG = enlarge (borderColour) g
    cs = getBoundingCoords g
    litCs = filter (\c -> enhance algo biggerG c == Light) cs
    s' = Set.fromList litCs
    newBorderColour = if borderColour == Dark then (algo 0) else algo 511

nTimes 0 _ = id
nTimes 1 f = f
nTimes n f = f . nTimes (n -1) f

solve algo image n = Set.size $ snd $ nTimes n (step algo') (Dark, g)
  where
    algoV = Vector.fromList algo
    algo' n = fromMaybe (error "oob") $ algoV Vector.!? n
    g = toSet image

solve1 (algo, image) = solve algo image 2

solve2 (algo, image) = solve algo image 50

solution =
  Solution
    { _parse = parseFile "20.txt" parseInts,
      _solve1 = solve1,
      _solve2 = solve2
    }
