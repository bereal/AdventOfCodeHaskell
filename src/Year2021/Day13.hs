{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Year2021.Day13 where

import Common (InputParser (ParsecParser), ShowAsIs (ShowAsIs), solveDay)
import Control.Applicative ((<|>))
import Control.Arrow (first, second)
import Control.DeepSeq (NFData)
import Data.Attoparsec.Text (char, decimal, endOfLine, sepBy1, string)
import Data.List (intercalate)
import qualified Data.Set as S
import GHC.Generics (Generic)

type Coord = (Int, Int)

type Paper = S.Set Coord

data Axis = X | Y deriving (Generic, NFData)

showPaper paper =
  let (maxX, maxY) = foldl (\(a, b) (c, d) -> (max a c, max b d)) (0, 0) paper
      itoc i = if i `S.member` paper then '#' else ' '
      row r = [itoc (i, r) | i <- [0 .. maxX]]
   in ShowAsIs $ "\n" ++ (intercalate "\n" [row r | r <- [0 .. maxY]]) ++ "\n"

coord = (,) <$> (decimal <* char ',') <*> decimal

paper = S.fromList <$> sepBy1 coord endOfLine

foldParser = ((X,) <$> (string "x=" *> decimal)) <|> ((Y,) <$> (string "y=" *> decimal))

folds = sepBy1 (string "fold along " *> foldParser) endOfLine

parser = ParsecParser $ (,) <$> paper <*> (endOfLine *> endOfLine *> folds)

get X = fst
get Y = snd

update X = first
update Y = second

fold :: Paper -> (Axis, Int) -> Paper
fold paper (axis, v) =
  let maxV = 2 * v
      (top, bottom) = S.partition ((< v) . get axis) paper
      bottom' = S.map (update axis (maxV -)) bottom
   in top `S.union` bottom'

solve1 (p, f : _) = S.size $ fold p f

solve2 (p, fs) = showPaper $ foldl fold p fs

solve = solveDay parser solve1 solve2
