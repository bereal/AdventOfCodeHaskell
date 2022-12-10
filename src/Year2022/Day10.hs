module Year2022.Day10 where
import Common (lineParser, solveDay, ShowAsIs (ShowAsIs))
import Data.Attoparsec.Text (signed, decimal, string)
import Control.Applicative ((<|>))
import Data.List.Split (chunksOf)
import Data.List (intercalate)

addx = Just <$> (string "addx " *> signed decimal)
noop = Nothing <$ string "noop"
parser = lineParser $ addx <|> noop

runProgram :: [Maybe Int] -> [Int]
runProgram = run 1 where
    run x (Nothing:t) = x : run x t
    run x (Just i:t) = x : x : run (x + i) t
    run _ [] = []

solve1 = sum . zipWith strength [1..] . runProgram where
    strength i x = if i == 20 || (i - 20) `mod` 40 == 0 then i * x else 0

solve2 = ShowAsIs . ('\n':) . intercalate "\n" . chunksOf 40 . zipWith pixel [1..] . runProgram where
    pixel i x = if abs (x - (i - 1) `mod` 40) < 2 then '#' else '.'

solve = solveDay parser solve1 solve2

