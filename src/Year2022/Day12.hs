{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE TupleSections #-}
module Year2022.Day12 where
import qualified Data.Array as A
import qualified Data.Set as S
import Data.Array ((!))
import Data.Char (ord)
import Common (lineParser, solveDay)
import Data.Attoparsec.Text (many1, letter)

type Node = (Int, Int)

type Map = A.Array Node Char

toMap :: [[Char]] -> Map
toMap xs =
    let (h, w) = (length xs, length $ head xs)
        in A.listArray ((0, 0), (h - 1, w - 1)) $ concat xs

score :: Char -> Int
score 'S' = ord 'a'
score 'E' = ord 'z'
score c = ord c

find c = fst . head . filter ((==c) . snd) . A.assocs

adjacent g n@(x,y) = let
        v = score $ g ! n
        neighbours = filter (A.inRange $ A.bounds g) [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
    in filter (\n -> v - score (g ! n) <= 1) neighbours

bfs' seen ((start, dist):rest) g end
  | g ! start == end = dist
  | S.member start seen = bfs' seen rest g end
  | otherwise = let neighbours = adjacent g start
                    seen' = S.insert start seen
                    new = filter (`S.notMember` seen) neighbours
                    rest' = rest ++ map (,dist+1) new
                in bfs' seen' rest' g end

bfs g start end = let startCoord = find start g in bfs' S.empty [(startCoord, 0)] g end

parser = toMap <$> lineParser (many1 letter)

solve1 :: Map -> Int
solve1 m = bfs m 'E' 'S'

solve2 :: Map -> Int
solve2 m = bfs m 'E' 'a'

solve = solveDay parser solve1 solve2