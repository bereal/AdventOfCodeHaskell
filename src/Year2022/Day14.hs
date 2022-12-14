{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Year2022.Day14 where
import Common (lineParser, solveDay)
import Data.Attoparsec.Text (Parser, decimal, char, sepBy1, string)
import qualified Data.HashSet as S
import Data.List (find)
import Data.Maybe (isJust, fromJust)

type Coord = (Int, Int)

coord :: Parser Coord
coord = (,) <$> (decimal <* char ',') <*> decimal
parser = expandToSet <$> lineParser (coord `sepBy1` string " -> ")

range a b | a < b = [a..b-1]
          | otherwise = [a,a-1..b+1]

expandSegment :: Coord -> Coord -> [Coord]
expandSegment (a, b) (c, d) | b == d = map (,b) $ range a c
                            | a == c = map (a,) $ range b d

expandSeq :: [Coord] -> [Coord]
expandSeq (a:t@(b:_)) = expandSegment a b ++ expandSeq t
expandSeq last = last

expandToSet :: [[Coord]] -> S.HashSet Coord
expandToSet = S.fromList . concatMap expandSeq

data Bottom = Floor | Void deriving (Show)
data Cave = Cave {cells :: S.HashSet Coord, depth :: Int, bottom :: Bottom} deriving (Show)

rest :: Coord -> Cave -> Maybe Cave
rest p c@Cave{bottom=Floor,..} = Just $ c {cells=S.insert p cells}
rest p@(_, y) c@Cave{..} = if y >= depth-1 then Nothing else Just $ c {cells=S.insert p cells}

free :: Coord -> Cave -> Bool
free p@(_, y) c@Cave{..} = not (p `S.member` cells || y >= depth)

fall :: Coord -> Cave -> Maybe Cave
fall p@(x, y) s = case find (`free` s) [(x, y + 1), (x - 1, y + 1), (x + 1, y + 1)] of
    Just p' -> fall p' s
    _ -> rest p s

iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f a = map fromJust $ takeWhile isJust $ iterate (f =<<) $ Just a

startPoint = (500, 0)

run :: Bottom -> S.HashSet Coord -> [Cave]
run b s = let d = 2 + maximum (S.map snd s) in iterateMaybe (fall startPoint) $ Cave s d b

solve1 = pred . length . map cells . run Void

solve2 = length . takeWhile (free startPoint) . run Floor

solve = solveDay parser solve1 solve2