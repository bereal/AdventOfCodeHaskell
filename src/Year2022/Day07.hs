{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Year2022.Day07 where
import Common (InputParser(ParsecParser), lineParser, solveDay)
import Control.DeepSeq (NFData)
import GHC.Generics
import Data.Attoparsec.Text (Parser, string, decimal, space, takeTill)
import Control.Applicative ((<|>))
import Data.Tuple (swap)
import Data.Text (Text, unpack)
import qualified Data.Map as M
import Data.List (sort)

-- Parsing

data Line = LS | CD Text | FileInfo Text Int | SubDir Text deriving (Generic, NFData)

takeTillEOL = takeTill ('\n' ==)

parseCmd :: Parser Line
parseCmd = (LS <$ string "ls") <|> (CD <$> (string "cd " *> takeTillEOL))

parseFile = uncurry FileInfo . swap <$> ((,) <$> (decimal <* space) <*> takeTillEOL)

parseSub = SubDir <$> (string "dir " *> takeTillEOL)

parser = lineParser $ (string "$ " *> parseCmd) <|> parseFile <|> parseSub


-- Solution

data Path = Root | Path Path Text deriving (Eq, Ord)

parent (Path p _) = p

parents = (++[Root]) . tail . takeWhile (/= Root) . iterate parent

type ShellState = (Path, M.Map Path Int)

runCmd :: ShellState -> Line -> ShellState
runCmd (_, m) (CD "/") = (Root, m)
runCmd (Path parent _, m) (CD "..") = (parent, m)
runCmd (p, m) (CD name) = (Path p name, m)
runCmd s LS = s
runCmd (p, m) (FileInfo name size) = (p, M.insert (Path p name) size m)
runCmd s (SubDir _) = s

run :: [Line] -> M.Map Path Int
run = snd . foldl runCmd (Root, M.empty)

dirSizes :: M.Map Path Int -> [Int]
dirSizes = M.elems . M.fromListWith (+) . concatMap expand . M.toList
    where expand (p, s) = [(p', s) | p' <- parents p]

solve1 = sum . filter (<=100000) . dirSizes . run

solve2 = find' . sort . dirSizes . run
    where find' sizes = let space = last sizes - 40000000 in head $ dropWhile (< space) sizes

solve = solveDay parser solve1 solve2
