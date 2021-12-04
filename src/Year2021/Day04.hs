{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Year2021.Day04 where

import Common (Input, InputParser (ParsecParser), solveDay)
import Data.Attoparsec.Text (char, decimal, endOfLine, many1, sepBy1, space)
import Data.IntMap ((!?))
import qualified Data.IntMap as IM
import Data.List (partition)
import Data.List.Split (chunksOf)

boardSize = 5

data Board = Board
  { numbers :: IM.IntMap (Int, Int),
    -- count called numbers in each row and column:
    rows :: IM.IntMap Int,
    cols :: IM.IntMap Int
  }

mkBoard :: [Int] -> Board
mkBoard nums =
  let coords = [(n, i `divMod` boardSize) | (n, i) <- zip nums [0 ..]]
   in Board (IM.fromList coords) IM.empty IM.empty

game = sepBy1 decimal (char ',') <* endOfLine

splitBoards = chunksOf (boardSize * boardSize)

boards = map mkBoard . splitBoards <$> sepBy1 decimal (many1 space)

parser = ParsecParser $ (,) <$> game <*> (endOfLine *> boards)

inc k = IM.insertWith (+) k 1

makeTurn :: Int -> Board -> Board
makeTurn n b@Board {..} = case numbers !? n of
  Just (row, col) -> Board (IM.delete n numbers) (inc row rows) (inc col cols)
  _ -> b

score Board {numbers} = sum $ IM.keys numbers

isWinner Board {rows, cols} = elem boardSize rows || elem boardSize cols

play :: [Int] -> [Board] -> [Int]
play _ [] = []
play (n : ns) boards =
  let boards' = map (makeTurn n) boards
      (winners, rest) = partition isWinner boards'
   in map ((* n) . score) winners ++ play ns rest

solve1, solve2 :: ([Int], [Board]) -> Int
solve1 = head . uncurry play
solve2 = last . uncurry play

solve = solveDay parser solve1 solve2
