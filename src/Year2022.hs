module Year2022 (days) where

import Client (ClientConfig, downloadInput)
import qualified Year2022.Day01 as Day01
import qualified Year2022.Day02 as Day02

days =
  [ Day01.solve,
    Day02.solve
  ]
