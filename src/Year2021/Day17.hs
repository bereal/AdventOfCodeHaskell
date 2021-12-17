{-# LANGUAGE NamedFieldPuns #-}

module Year2021.Day17 where

import Common (InputParser (ParsecParser), solveDay)
import Data.Attoparsec.Text (Parser, decimal, signed, string)
import Data.Maybe (catMaybes, isJust)

data Probe = Probe {pos :: (Int, Int), v :: (Int, Int), record :: Int} deriving (Show)

type Target = ((Int, Int), (Int, Int))

range :: Parser (Int, Int)
range = (,) <$> (signed decimal <* string "..") <*> signed decimal

parser = ParsecParser $ (,) <$> (string "target area: x=" *> range) <*> (string ", y=" *> range)

inTarget ((x1, x2), (y1, y2)) (x, y) = x1 <= x && x <= x2 && y1 <= y && y <= y2

overTarget ((_, x2), (y1, _)) (x, y) = x > x2 || y < y1

moveProbe :: Probe -> Probe
moveProbe (Probe (x, y) (vx, vy) r) = let x' = x + vx; y' = y + vy in Probe (x', y') (max 0 $ vx - 1, vy - 1) (max r y')

launch :: Target -> (Int, Int) -> Maybe Int
launch t v = launch' $ Probe (0, 0) v 0
  where
    launch' p@Probe {pos, record}
      | inTarget t pos = Just record
      | overTarget t pos = Nothing
      | otherwise = launch' $ moveProbe p

bruteForce target@((_, x2), (y1, _)) = [launch target (x, y) | x <- [1 .. x2], y <- [y1 .. 500]]

solve1 = maximum . catMaybes . bruteForce

solve2 = length . filter isJust . bruteForce

solve = solveDay parser solve1 solve2
