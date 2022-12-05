module Year2022.Day05 where

import Common (InputParser (ParsecParser), solveDay)
import Data.Attoparsec.Text (Parser, char, letter, string, decimal,anyChar, endOfLine, count, sepBy1)
import Control.Applicative ((<|>))
import Data.List (transpose)
import Data.Maybe (catMaybes)
import Data.IntMap (IntMap, (!), fromDistinctAscList, adjust, insert, elems)

type State = IntMap String  -- columns
type Move = (Int, Int, Int) -- quantity, from, to

-- Parsing

space = char ' '

boxParser :: Parser (Maybe Char)
boxParser = Just <$> (char '[' *> letter <* char ']') <|> Nothing <$ (space *> count 2 anyChar)

row = boxParser `sepBy1` space

boxes :: Parser State
boxes = fromDistinctAscList . zip [1..] . map catMaybes . transpose <$> row `sepBy1` endOfLine

move :: Parser Move
move = (,,) <$> (string "move " *> decimal) <*> (string " from " *> decimal) <*> (string " to " *> decimal)

parser :: InputParser (State, [Move])
parser = ParsecParser $ (,) <$> boxes <*> (count 2 endOfLine *> (move `sepBy1` endOfLine))


-- Solution

step :: (String -> String) -> State -> Move -> State
step transform state (n, from, to) = let (top, bottom) = splitAt n $ state ! from in
    insert from bottom $ adjust (transform top++) to state

solve' transform (state, moves) = map head $ elems $ foldl (step transform) state moves

solve = solveDay parser (solve' id) (solve' reverse)