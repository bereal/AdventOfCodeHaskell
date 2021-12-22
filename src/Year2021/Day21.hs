{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Year2021.Day21 where

import Common (InputParser (ParsecParser), solveDay)
import Control.Arrow (first)
import Data.Attoparsec.Text (decimal, endOfLine, string)
import Data.Map.Strict ((!?))
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, isNothing)

position = seq <$> (string "Player " *> decimal) <*> (string " starting position: " *> decimal)

parser = ParsecParser $ (,) <$> (position <* endOfLine) <*> position

type Player = (Int, Int)

move :: Int -> Player -> Player
move i (pos, score) =
  let pos' = 1 + ((pos + i - 1) `mod` 10)
   in (pos', score + pos')

type Game = (Player, Player, Int)

initGame :: (Int, Int) -> Game
initGame (a, b) = ((a, 0), (b, 0), 0)

makeTurn :: Game -> Int -> Game
makeTurn (p1, p2, i) roll =
  if even i
    then let p1' = move roll p1 in (p1', p2, i + 1)
    else let p2' = move roll p2 in (p1, p2', i + 1)

makeTurn2 :: Game -> Int -> Game
makeTurn2 (p1, p2, 0) roll = let p1' = move roll p1 in (p1', p2, 1)
makeTurn2 (p1, p2, 1) roll = let p2' = move roll p2 in (p1, p2', 0)

makeDieTurn :: (Game, [Int]) -> (Game, [Int])
makeDieTurn (game, die) =
  let (rolls, die') = splitAt 3 die
      game' = makeTurn game $ sum rolls
   in (game', die')

finalScore :: Game -> Maybe Int
finalScore ((_, s1), (_, s2), turns)
  | s1 >= 1000 = Just $ 3 * s2 * turns
  | s2 >= 1000 = Just $ 3 * s1 * turns
  | otherwise = Nothing

-- possible sums of the 3 3-sided die's rolls
-- with the number of possible rolls leading to that sum
diracRolls = [(3, 1), (4, 3), (5, 6), (6, 7), (7, 6), (8, 3), (9, 1)]

type Memo = M.Map Game (Int, Int)

playDirac :: Memo -> Game -> (Memo, (Int, Int))
playDirac memo game@(player1@(p1, s1), player2@(p2, s2), turn)
  | s2 >= 21 = (memo, (0, 1))
  | s1 >= 21 = (memo, (1, 0))
  | otherwise = case memo !? game of
    Just v -> (memo, v)
    _ ->
      let subGames = map (first (makeTurn2 game)) diracRolls
          update (m, (r1, r2)) (sg, c) =
            let (m', (a, b)) = playDirac m sg in (m', (r1 + c * a, r2 + c * b))
          (memo', result) = foldl update (memo, (0, 0)) subGames
       in (M.insert game result memo', result)

solve1 =
  fromJust
    . head
    . dropWhile isNothing
    . map (finalScore . fst)
    . iterate makeDieTurn
    . (,cycle [1 .. 100])
    . initGame

solve2 = uncurry max . snd . playDirac M.empty . initGame

solve = solveDay parser solve1 solve2
