module Year2022 (days) where

import qualified Year2022.Day01 as Day01
import qualified Year2022.Day02 as Day02
import qualified Year2022.Day03 as Day03
import qualified Year2022.Day04 as Day04
import qualified Year2022.Day05 as Day05
import qualified Year2022.Day06 as Day06
import qualified Year2022.Day07 as Day07
import qualified Year2022.Day08 as Day08
import qualified Year2022.Day09 as Day09
import qualified Year2022.Day10 as Day10
import qualified Year2022.Day11 as Day11
import Common (skipDay)


days =
  [ Day01.solve,
    Day02.solve,
    Day03.solve,
    Day04.solve,
    Day05.solve,
    Day06.solve,
    Day07.solve,
    Day08.solve,
    Day09.solve,
    Day10.solve,
    Day11.solve
  ]
