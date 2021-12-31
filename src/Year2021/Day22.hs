{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Year2021.Day22 where

import Common (Input, InputParser (ParsecParser), skipDay, solveDay)
import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
import Data.Attoparsec.Text (Parser, anyChar, char, decimal, endOfInput, endOfLine, letter, many1, sepBy1, signed, string)
import Data.HashSet as S
import Data.Maybe (fromJust, isJust, mapMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)

type Range = (Int, Int)

type Cube = (Range, Range, Range)

type Command = (Bool, Cube)

coeff = signed decimal

range :: Parser (Int, Int)
range = (,) <$> (letter *> char '=' *> coeff) <*> (string ".." *> coeff)

triple [a, b, c] = (a, b, c)

cube = triple <$> sepBy1 range (char ',')

command = ((True,) <$> (string "on " *> cube)) <|> ((False,) <$> (string "off " *> cube))

parser = ParsecParser $ sepBy1 command endOfLine

cutRange :: Range -> Maybe Range
cutRange (a, b)
  | b < -50 = Nothing
  | a > 50 = Nothing
  | otherwise = Just (max a (-50), min b 50)

cutCube :: Cube -> Maybe Cube
cutCube (a, b, c) = case (cutRange a, cutRange b, cutRange c) of
  (Just a, Just b, Just c) -> Just (a, b, c)
  _ -> Nothing

cutCmd :: Command -> Maybe Command
cutCmd (v, c) = (v,) <$> cutCube c

enumCube :: Cube -> S.HashSet (Int, Int, Int)
enumCube ((a1, a2), (b1, b2), (c1, c2)) = S.fromList [(a, b, c) | a <- [a1 .. a2], b <- [b1 .. b2], c <- [c1 .. c2]]

solve1 cmds =
  let cs = mapMaybe cutCmd cmds
      upd s (True, cube) = S.union s (enumCube cube)
      upd s (False, cube) = S.difference s (enumCube cube)
      s = foldl upd S.empty cs
   in S.size s

solve = solveDay parser solve1 (const (0 :: Int))
