{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Year2021.Day12 where

import Common (InputParser (ParsecParser), solveDay)
import Control.Applicative ((<|>))
import Data.Attoparsec.Text (char, endOfLine, letter, many1, sepBy1)
import Data.Char (isAsciiUpper, ord)
import Data.IntMap ((!))
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

type Map = IM.IntMap [Int]

type Memory = IS.IntSet

-- Large caves are odd, small caves are even
nodeId "start" = 0
nodeId "end" = -1
nodeId s@(a : _) = let v = foldl1 ((+) . (* 0x100)) (map ord s) in 2 * v + fromEnum (isAsciiUpper a)

node = nodeId <$> many1 letter

route = (,) <$> node <*> (char '-' *> node)

mkMap :: [(Int, Int)] -> Map
mkMap = IM.fromListWith (++) . concatMap (\(a, b) -> [(a, [b]), (b, [a])])

parser = ParsecParser $ mkMap <$> sepBy1 route endOfLine

countPaths :: Int -> Memory -> Bool -> Map -> Int
countPaths start mem allowRepeat m =
  let choose 0 = 0
      choose (-1) = 1
      choose n@(even -> True)
        | (n `IS.notMember` mem) = countPaths n (IS.insert n mem) allowRepeat m
        | allowRepeat = countPaths n mem False m
        | otherwise = 0
      choose n = countPaths n mem allowRepeat m
   in sum $ map choose (m ! start)

solve' :: Bool -> Map -> Int
solve' = countPaths 0 IS.empty

solve1, solve2 :: Map -> Int
solve1 = solve' False
solve2 = solve' True

solve = solveDay parser solve1 solve2
