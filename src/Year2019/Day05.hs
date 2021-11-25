{-# LANGUAGE NamedFieldPuns #-}

module Year2019.Day05 where

import Common (Input, InputParser (ParsecParser), solveDay)
import Control.Applicative ((<|>))
import Data.Attoparsec.Text (Parser, char, decimal, digit, sepBy1)
import Year2019.Intcode (ProgramState (ProgramState, output), initProgram, runProgramWithInput)

int = decimal <|> negate <$> (char '-' *> decimal)

parser = ParsecParser $ sepBy1 int (char ',')

solve' input p =
  let s = initProgram p
      ProgramState {output} = runProgramWithInput input s
   in head output

solve = solveDay parser (solve' [1]) (solve' [5])
