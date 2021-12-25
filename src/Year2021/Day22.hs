{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Year2021.Day22 where

import Common (Input, InputParser (ParsecParser), skipDay, solveDay)
import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)
import Data.Attoparsec.Text (Parser, anyChar, char, decimal, endOfInput, endOfLine, letter, many1, sepBy1, signed, string)
import Data.Maybe (fromJust, isJust)
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

-- cutCube :: Cube -> Maybe Cube

-- filterCommands = map (fromJust <$>) . filter (isJust . snd) . map (cutRange <$>)

solve = solveDay parser id id
