{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Year2021.Day10 where

import Common (InputParser (ParsecParser), solveDay)
import Data.Attoparsec.Text (char, choice, endOfLine, many1, sepBy1)
import Data.Either (lefts, rights)
import Data.List (sort)

data Bracket = Open Char | Close Char

pair [o, c] = [Open <$> char o, Close o <$ char c]

bracket = choice $ concatMap pair ["()", "[]", "{}", "<>"]

parser = ParsecParser $ sepBy1 (many1 bracket) endOfLine

analyze :: [Bracket] -> Either Char String
analyze = run []
  where
    run stack [] = Right stack
    run stack (Open c : cs) = run (c : stack) cs
    run [] (Close c : cs) = Left c
    run (top : stack) (Close c : cs)
      | c == top = run stack cs
      | otherwise = Left c

errCost c = case c of '(' -> 3; '[' -> 57; '{' -> 1197; '<' -> 25137

closingCost c = case c of '(' -> 1; '[' -> 2; '{' -> 3; '<' -> 4

completionCost = foldl (\a b -> 5 * a + closingCost b) 0

middle xs = xs !! (length xs `div` 2)

solve1 = sum . map errCost . lefts . map analyze

solve2 = middle . sort . map completionCost . rights . map analyze

solve = solveDay parser solve1 solve2
