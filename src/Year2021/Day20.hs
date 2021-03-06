{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Year2021.Day20 where

import Common (InputParser (ParsecParser), solveDay)
import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
import Data.Attoparsec.Text (char, endOfLine, many1, sepBy1)
import Data.Bits (shift, xor)
import Data.Functor (($>))
import qualified Data.HashSet as S
import Data.Vector ((!))
import qualified Data.Vector as V
import GHC.Generics (Generic)

type Code = V.Vector Bool

type Cell = (Int, Int)

data Image = Image {cells :: S.HashSet Cell, inverted :: Bool} deriving (Generic, NFData)

(!~) :: Image -> Cell -> Int
(!~) Image {..} cell =
  let pixel p = fromEnum $ p `S.member` cells
      v = bitsToInt (map pixel $ neighbours cell)
   in if inverted then v `xor` 0x1ff else v

pixel = (char '.' $> 0) <|> (char '#' $> 1)

code = V.fromList . map toEnum <$> many1 pixel <* endOfLine

image = toImage <$> sepBy1 (many1 pixel) endOfLine

toImage :: [[Int]] -> Image
toImage img =
  let row r n = [(n, i) | (c, i) <- zip r [0 ..], c /= 0]
      s = S.fromList $ concat $ zipWith row img [0 ..]
   in Image s False

parser = ParsecParser $ (,) <$> (code <* endOfLine) <*> image

bitsToInt :: [Int] -> Int
bitsToInt = foldl ((+) . (`shift` 1)) 0

neighbours (a, b) = [(a + i, b + j) | i <- [-1 .. 1], j <- [-1 .. 1]]

closure :: S.HashSet Cell -> [(Int, Int)]
closure s =
  let upd (a1, a2, b1, b2) (a, b) = (min a1 a, max a2 a, min b1 b, max b2 b)
      (a1, a2, b1, b2) = foldl upd (0, 0, 0, 0) $ S.toList s
   in [(a, b) | a <- [a1 - 1 .. a2 + 1], b <- [b1 - 1 .. b2 + 1]]

expand :: Code -> Image -> Image
expand code img@Image {..} =
  let cells' = closure cells
      inverted' = code ! 0 /= inverted
      update cell = (code ! (img !~ cell)) /= inverted'
      updated = S.fromList $ filter update cells'
   in Image updated inverted'

expandAndCount code img steps = S.size $ cells $ iterate (expand code) img !! steps

solve' steps (code, img) = S.size $ cells $ iterate (expand code) img !! steps

solve = solveDay parser (solve' 2) (solve' 50)
