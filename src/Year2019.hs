module Year2019 (days) where

import Client (ClientConfig, downloadInput)
import qualified Year2019.Day01 as Day01
import qualified Year2019.Day02 as Day02
import qualified Year2019.Day03 as Day03

days = [Day01.solve, Day02.solve, Day03.solve]
