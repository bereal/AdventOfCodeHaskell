module Year2025 (days) where

import Common (skipDay)
import qualified Year2025.Day01 as Day01
import qualified Year2025.Day02 as Day02
import qualified Year2025.Day03 as Day03
import qualified Year2025.Day04 as Day04
import qualified Year2025.Day05 as Day05

days =
  [Day01.solve, Day02.solve, Day03.solve, Day04.solve, Day05.solve]
