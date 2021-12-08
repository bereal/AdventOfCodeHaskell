module Year2021.Day08 where

import Common (InputParser (ParsecParser), solveDay)
import Control.Arrow ((&&&))
import Data.Attoparsec.Text (char, choice, endOfLine, many1, sepBy1, string)
import Data.List (sortOn)
import Data.Set ((\\))
import qualified Data.Set as S

word = many1 (choice $ map char "abcdefg")

part = sepBy1 word $ char ' '

record = (,) <$> (part <* string " | ") <*> part

parser :: InputParser [Record]
parser = ParsecParser $ sepBy1 record endOfLine

-- digits:
-- 0: abcefg 1: cf 2: acdeg 3: acdfg 4: bcdf 5: abdfg 6: abdefg 7: acf 8: abcdefg 9: abcdfg

type Record = ([String], [String]) -- lhs, rhs

type Fingerprint = String -> (Bool, Bool)

isSimple = (`elem` [2, 3, 4, 7]) . length

resolveDigit :: Fingerprint -> String -> Int
resolveDigit fp digit = resolve' (length digit)
  where
    resolve' 5 = case fp digit of (True, _) -> 5; (_, True) -> 2; _ -> 3
    resolve' 6 = case fp digit of (True, True) -> 6; (True, False) -> 9; _ -> 0
    resolve' len = case len of 2 -> 1; 3 -> 7; 4 -> 4; _ -> 8

fingerprint :: [String] -> Fingerprint
fingerprint s =
  let [d1, d7, d4, d8] = map S.fromList $ sortOn length $ filter isSimple s
      bd = d4 \\ d1
      eg = d8 \\ foldl1 S.union [d1, d4, d7]
   in (S.isSubsetOf bd &&& S.isSubsetOf eg) . S.fromList

solveRecord :: Record -> [Int]
solveRecord (lhs, rhs) =
  let fp = fingerprint lhs
   in map (resolveDigit fp) rhs

solve1 = length . filter isSimple . concatMap snd

toNum = foldl1 ((+) . (* 10))

solve2 = sum . map (toNum . solveRecord)

solve = solveDay parser solve1 solve2
