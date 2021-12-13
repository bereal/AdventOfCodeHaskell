{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Year2021.Day13 where

import Common (InputParser (ParsecParser), solveDay)
import Control.Applicative ((<|>))
import Control.Arrow (first, second)
import Control.DeepSeq (NFData)
import Data.Attoparsec.Text (char, decimal, endOfLine, sepBy1, string)
import Data.List (intercalate)
import qualified Data.Set as S
import GHC.Generics (Generic)

type Coord = (Int, Int)

newtype Paper = Paper (S.Set Coord) deriving (Generic, NFData)

data Axis = X | Y deriving (Generic, NFData)

instance Show Paper where
  show = showPaper

showPaper (Paper dots) =
  let (maxX, maxY) = foldl (\(a, b) (c, d) -> (max a c, max b d)) (0, 0) dots
      itoc ((`S.member` dots) -> False) = '.'
      itoc _ = '#'
      row r = [itoc (i, r) | i <- [0 .. maxX]]
   in "\n" ++ (intercalate "\n" [row r | r <- [0 .. maxY]]) ++ "\n"

coord = (,) <$> (decimal <* char ',') <*> decimal

paper = Paper . S.fromList <$> sepBy1 coord endOfLine

foldParser = ((X,) <$> (string "x=" *> decimal)) <|> ((Y,) <$> (string "y=" *> decimal))

folds = sepBy1 (string "fold along " *> foldParser) endOfLine

parser = ParsecParser $ (,) <$> paper <*> (endOfLine *> endOfLine *> folds)

get X = fst
get Y = snd

update X = first
update Y = second

fold :: Paper -> (Axis, Int) -> Paper
fold (Paper dots) (axis, v) =
  let maxV = 2 * v
      (top, bottom) = S.partition ((< v) . get axis) $ S.filter ((/= v) . get axis) dots
      bottom' = S.map (update axis (maxV -)) bottom
   in Paper $ top `S.union` bottom'

solve1 (p, f : _) = let (Paper s) = fold p f in S.size s

solve2 (p, fs) = foldl fold p fs

solve = solveDay parser solve1 solve2
