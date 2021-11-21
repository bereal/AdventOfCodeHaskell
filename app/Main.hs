{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Client (ClientConfig, initOrReadConfig)
import Common (DayRunner, Input (HTTPInput))
import Data.Map ((!))
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe)
import Options.Applicative
  ( Parser,
    ReadM,
    auto,
    eitherReader,
    execParser,
    fullDesc,
    info,
    long,
    option,
    short,
    value,
  )
import System.Environment (getArgs)
import Text.Printf (printf)
import Text.Read (readEither)
import qualified Year2021

years = M.fromList [(2021, Year2021.days)]

run config year day = do
  let yearDays = years ! year
  printf "== Day %02d ==========\n\n" day
  let solve = yearDays !! (day - 1)
   in solve $ HTTPInput config year day
  putStrLn "\n===================="

data ParsedFlags = ParsedFlags
  { fBasedir :: Maybe FilePath,
    fYear :: Int,
    fDay :: Maybe Int
  }

optionalReader :: Read a => ReadM (Maybe a)
optionalReader = eitherReader (fmap Just . readEither)

flagParser :: Parser ParsedFlags
flagParser =
  ParsedFlags
    <$> option optionalReader (short 'b' <> long "basedir" <> value Nothing)
    <*> option auto (short 'y' <> long "year" <> value 2021)
    <*> option optionalReader (short 'd' <> long "day" <> value Nothing)

parseFlags = do
  args <- getArgs
  execParser $ info flagParser fullDesc

main = do
  args <- getArgs
  ParsedFlags {fBasedir, fYear, fDay} <- parseFlags
  config <- initOrReadConfig fBasedir
  let days = maybe [1 .. length (years ! fYear)] (: []) fDay
  mapM_ (run config fYear) days
