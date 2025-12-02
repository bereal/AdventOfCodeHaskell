module Year2025 (days) where

import Common (skipDay)
import qualified Year2025.Day01 as Day01
import qualified Year2025.Day02 as Day02

days =
  [Day01.solve, Day02.solve]
