module Year2022.Day03 where

import Common (InputParser (ParsecParser), solveDay)
import Data.Attoparsec.Text (endOfLine, sepBy1, many1, letter)
import Data.Char (ord, isAsciiLower)
import Data.List (intersect)
import Data.List.Split (chunksOf)

parser :: InputParser [String]
parser = ParsecParser $ sepBy1 (many1 letter) endOfLine

priority a | isAsciiLower a = ord a - ord 'a' + 1
           | otherwise = ord a - ord 'A' + 27

splitHalf s = splitAt (length s `div` 2) s

solve1 = sum . map (priority . head . uncurry intersect . splitHalf)

solve2 = sum . map (priority . head . foldr1 intersect) . chunksOf 3

solve = solveDay parser solve1 solve2