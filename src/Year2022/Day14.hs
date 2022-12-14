{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Year2022.Day14 where
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Common (lineParser, solveDay)
import Data.Attoparsec.Text (Parser, decimal, char, sepBy1, string)
import qualified Data.HashSet as S
import Data.List (find)

type Coord = (Int, Int)

coord :: Parser Coord
coord = (,) <$> (decimal <* char ',') <*> decimal
parser = toCave <$> lineParser (coord `sepBy1` string " -> ")

range a b | a < b = [a..b-1]
          | otherwise = [a,a-1..b+1]

expandSegment :: Coord -> Coord -> [Coord]
expandSegment (a, b) (c, d) | b == d = map (,b) $ range a c
                            | a == c = map (a,) $ range b d

expandSeq :: [Coord] -> [Coord]
expandSeq (a:t@(b:_)) = expandSegment a b ++ expandSeq t
expandSeq last = last

toCave :: [[Coord]] -> Cave
toCave cs = let s = S.fromList $ concatMap expandSeq cs
    in Cave s $ 2 + maximum (S.map snd s)

data Cave = Cave {cells :: S.HashSet Coord, depth :: Int} deriving (NFData, Generic)

rest :: Coord -> Cave -> Cave
rest p c@Cave{..} = c {cells=S.insert p cells}

free :: Coord -> Cave -> Bool
free p@(_, y) c@Cave{..} = not (p `S.member` cells || y >= depth)

fall :: Coord -> Cave -> Coord
fall p@(x, y) s = case find (`free` s) [(x, y + 1), (x - 1, y + 1), (x + 1, y + 1)] of
    Just p' -> fall p' s
    _ -> p

startPoint = (500, 0)

run :: Cave -> [Coord]
run cave = let p = fall startPoint cave in (p : run (rest p cave))

solve1 c@Cave{..} = length $ takeWhile ((< depth) . succ . snd) $ run c

solve2 = succ . length . takeWhile (startPoint /=) . run

solve = solveDay parser solve1 solve2