module Year2019.Day09 where

import Common (Input, InputParser (HardCoded, ParsecParser), skipDay, solveDay)
import Data.Attoparsec.Text (char, decimal, sepBy1, signed)
import Data.Text (Text)
import Year2019.Intcode (ProgramState (ProgramState, input, output), initProgram, runProgramWithInput)

parser = ParsecParser $ sepBy1 (signed decimal) (char ',')

run input code =
  let p = initProgram code
      ProgramState {output = [x]} = runProgramWithInput [input] p
   in x

solve = solveDay parser (run 1) (run 2)
