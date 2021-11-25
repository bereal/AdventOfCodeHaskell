{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Year2019.Day03 where

import Common (Input, InputParser (ParsecParser), solveDay)
import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser, char, decimal, endOfLine, sepBy1)
import Data.Foldable (minimumBy)
import Data.Functor (($>))
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import qualified Data.Set as S
import Data.Text (Text)

parser = ParsecParser $ (,) <$> (wire <* endOfLine) <*> wire

wire = sepBy1 segment (char ',')

segment :: Parser (Int, Int)
segment = do
  (x, y) <- direction
  l <- decimal
  return (x * l, y * l)

direction :: Parser (Int, Int)
direction = dir 'R' (1, 0) <|> dir 'L' (-1, 0) <|> dir 'U' (0, 1) <|> dir 'D' (0, -1)
  where
    dir = ($>) . char

add (a, b) (c, d) = (a + c, b + d)

unrollSegment :: (Int, Int) -> [(Int, Int)]
unrollSegment (x, y) = replicate (abs x + abs y) (signum x, signum y)

traceWire :: [(Int, Int)] -> [(Int, Int)]
traceWire = tail . scanl add (0, 0) . concatMap unrollSegment

solve1 (w1, w2) =
  let t1 = traceWire w1
      t2 = traceWire w2
      intersections = HS.fromList t1 `HS.intersection` HS.fromList t2
      dist = (\(x, y) -> abs x + abs y) <$> HS.toList intersections
   in minimum dist

solve2 (w1, w2) =
  let t1 = traceWire w1
      t2 = traceWire w2
      hm1 = HM.fromList $ zip t1 [1 ..]
      hm2 = HM.fromList $ zip t2 [1 ..]
      dists = HM.intersectionWith (+) hm1 hm2
   in minimum dists

solve = solveDay parser solve1 solve2
