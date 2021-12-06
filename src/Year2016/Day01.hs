-- Day 1: No Time for a Taxicab
-- https://adventofcode.com/2016/day/1
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Year2016.Day01 where

import Common (InputParser (ParsecParser), solveDay)
import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser, char, decimal, endOfLine, sepBy1, string)
import Data.Functor (($>))
import qualified Data.Set as S
import Data.Tuple (swap)

-- This is written in this convoluted way on purpose, just to check a concept
-- All actions are parsed straight to functions transforming the cab position

type Vector = (Int, Int)

-- (position, direction)
type State = (Vector, Vector)

rotation :: Parser (State -> State)
rotation = fmap <$> ((char 'R' $> swap . fmap negate) <|> (char 'L' $> fmap negate . swap))

move :: Int -> State -> [State]
move n ((x, y), (a, b)) = [((x + a * i, y + b * i), (a, b)) | i <- [1 .. n]]

command :: Parser (State -> [State])
command = flip (.) <$> rotation <*> (move <$> decimal)

distance (x, y) = abs x + abs y

parser :: InputParser [State -> [State]]
parser = ParsecParser $ sepBy1 command (string ", ")

run [a] (f : fs) = run (f a) fs
run (a : as) fs = a : run as fs
run _ [] = []

startPoint = [((0, 0), (0, 1))]

solve1 = distance . fst . last . run startPoint

findRepeating :: Ord a => [a] -> a
findRepeating = find' S.empty
  where
    find' s (n : ns) = if n `S.member` s then n else find' (S.insert n s) ns

solve2 = distance . findRepeating . map fst . run startPoint

solve = solveDay parser solve1 solve2
