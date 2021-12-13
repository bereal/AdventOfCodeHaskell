{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Year2021.Day13 where

import Common (InputParser (ParsecParser), solveDay)
import Control.Applicative ((<|>))
import Control.Arrow (Arrow (second), first)
import Control.DeepSeq (NFData)
import Data.Attoparsec.Text (char, decimal, endOfLine, sepBy1, string)
import Data.List (intercalate)
import qualified Data.Set as S
import GHC.Generics (Generic)

type Coord = (Int, Int)

data Paper = Paper {dots :: S.Set Coord, maxX :: Int, maxY :: Int} deriving (Generic, NFData)

instance Show Paper where
  show = showPaper

showPaper Paper {..} =
  let itoc ((`S.member` dots) -> False) = '.'
      itoc _ = '#'
      row r = [itoc (i, r) | i <- [0 .. maxX]]
   in "\n" ++ (intercalate "\n" [row r | r <- [0 .. maxY]]) ++ "\n"

coord = (,) <$> (decimal <* char ',') <*> decimal

data Fold = X Int | Y Int deriving (Generic, NFData)

toPaper :: [Coord] -> Paper
toPaper xs =
  let (x, y) = foldl (\(a, b) (c, d) -> (max a c, max b d)) (0, 0) xs
   in Paper (S.fromList xs) x y

paper = toPaper <$> sepBy1 coord endOfLine

foldParser = (X <$> (string "x=" *> decimal)) <|> (Y <$> (string "y=" *> decimal))

folds = sepBy1 (string "fold along " *> foldParser) endOfLine

parser = ParsecParser $ (,) <$> paper <*> (endOfLine *> endOfLine *> folds)

flipHoriz, flipVert :: Paper -> Paper
flipHoriz p@Paper {..} = p {dots = S.map (first (maxX -)) dots}
flipVert p@Paper {..} = p {dots = S.map (second (maxY -)) dots}

cutX, cutY :: Int -> Paper -> (Paper, Paper)
cutX x p@Paper {dots, maxX} =
  let left = p {dots = S.filter ((< x) . fst) dots, maxX = x - 1}
      right = p {dots = S.map (first $ subtract (x + 1)) $ S.filter ((>= x) . fst) dots, maxX = maxX - x - 1}
   in (left, right)
cutY y p@Paper {dots, maxY} =
  let top = p {dots = S.filter ((< y) . snd) dots, maxY = y - 1}
      bottom = p {dots = S.map (second $ subtract (y + 1)) $ S.filter ((>= y) . snd) dots, maxY = maxY - y - 1}
   in (top, bottom)

merge :: Paper -> Paper -> Paper
merge p1@Paper {dots = d1, maxX = x1, maxY = y1} p2@Paper {dots = d2, maxX = x2, maxY = y2} =
  Paper {dots = d1 `S.union` d2, maxX = max x1 x2, maxY = max y1 y2}

fold :: Paper -> Fold -> Paper
fold p (X x) =
  let (l, r) = cutX x p in flipHoriz $ merge (flipHoriz l) r
fold p (Y y) =
  let (u, d) = cutY y p in flipVert $ merge (flipVert u) d

solve1 :: (Paper, [Fold]) -> Int
solve1 (p, f : _) = S.size $ dots $ fold p f

solve2 :: (Paper, [Fold]) -> Paper
solve2 (p, fs) = foldl fold p fs

solve = solveDay parser solve1 solve2
