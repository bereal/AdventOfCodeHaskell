module Year2019 (days) where

import Client (ClientConfig, downloadInput)
import qualified Year2019.Day01 as Day01
import qualified Year2019.Day02 as Day02
import qualified Year2019.Day03 as Day03
import qualified Year2019.Day04 as Day04
import qualified Year2019.Day05 as Day05
import qualified Year2019.Day06 as Day06
import qualified Year2019.Day07 as Day07
import qualified Year2019.Day08 as Day08
import qualified Year2019.Day09 as Day09

days =
  [ Day01.solve,
    Day02.solve,
    Day03.solve,
    Day04.solve,
    Day05.solve,
    Day06.solve,
    Day07.solve,
    Day08.solve,
    Day09.solve
  ]
