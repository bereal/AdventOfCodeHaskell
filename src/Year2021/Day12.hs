module Year2021.Day12 where

import Common (InputParser (ParsecParser), solveDay)
import Control.Applicative ((<|>))
import Data.Attoparsec.Text (char, endOfLine, inClass, many1, satisfy, sepBy1, string)
import Data.Map ((!))
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text, pack)

data Node = Start | End | Small Text | Large Text deriving (Eq, Ord)

type Map = M.Map Node [Node]

type Memory = S.Set Node

textInClass s = pack <$> many1 (satisfy $ inClass s)

node =
  Start <$ string "start"
    <|> End <$ string "end"
    <|> Small <$> textInClass "a-z"
    <|> Large <$> textInClass "A-Z"

route = (,) <$> node <*> (char '-' *> node)

mkMap :: [(Node, Node)] -> Map
mkMap = M.fromListWith (++) . concatMap (\(a, b) -> [(a, [b]), (b, [a])])

parser = ParsecParser $ mkMap <$> sepBy1 route endOfLine

countPaths :: Node -> Memory -> Bool -> Map -> Int
countPaths start mem allowRepeat m =
  let choose Start = 0
      choose End = 1
      choose n@(Small _)
        | (n `S.notMember` mem) = countPaths n (S.insert n mem) allowRepeat m
        | allowRepeat = countPaths n mem False m
        | otherwise = 0
      choose n = countPaths n mem allowRepeat m
   in sum $ map choose (m ! start)

solve' :: Bool -> Map -> Int
solve' = countPaths Start S.empty

solve1, solve2 :: Map -> Int
solve1 = solve' False
solve2 = solve' True

solve = solveDay parser solve1 solve2
