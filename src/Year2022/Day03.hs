module Year2022.Day03 where

import Common (InputParser (ParsecParser), solveDay)
import Data.Attoparsec.Text (endOfLine, sepBy1, many1, letter)
import qualified Data.Set as S
import Data.Char (ord)
import Data.List.Split (chunksOf)

parser :: InputParser [String]
parser = ParsecParser $ sepBy1 (many1 letter) endOfLine

priority a = let
    lowerA = ord 'a'
    upperA = ord 'A'
    p = ord a in
      if p >= lowerA then p - lowerA + 1 else p - upperA + 27

splitHalf s = splitAt (length s `div` 2) s

solve1 = sum . map (sum . map priority . common . splitHalf)
    where
        common (l, r) = S.toList $ S.intersection (S.fromList l) (S.fromList r)

solve2 = sum . map (priority . badge) . chunksOf 3 where
    badge = head . S.toList . foldr1 S.intersection . map S.fromList

solve = solveDay parser solve1 solve2