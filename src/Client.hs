{-# LANGUAGE NamedFieldPuns #-}

module Client where

import Control.Exception (bracket_, catch, throw)
import Control.Monad.Catch (MonadThrow, handleIOError, try)
import Data.Maybe (fromMaybe)
import Data.String (IsString (fromString))
import Data.Text (Text)
import qualified Data.Text.IO as IO
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Network.HTTP.Client
  ( Request (requestHeaders),
    Response (responseBody, responseStatus),
    defaultManagerSettings,
    httpLbs,
    newManager,
    parseRequest,
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (Status (Status, statusCode))
import System.Directory (createDirectoryIfMissing, doesFileExist, getCurrentDirectory, getHomeDirectory)
import System.FilePath ((</>))
import System.IO (hFlush, hGetEcho, hSetEcho, stdin, stdout)
import Text.Printf (printf)

data ClientConfig = ClientConfig
  { basedir :: String,
    sessionId :: String
  }

getBasedir :: Maybe String -> IO FilePath
getBasedir (Just s) = return s
getBasedir _ = (</> ".advent-of-code") <$> getHomeDirectory

withEcho :: Bool -> IO a -> IO a
withEcho echo action = do
  old <- hGetEcho stdin
  bracket_ (hSetEcho stdin echo) (hSetEcho stdin old) action

readConfig :: FilePath -> IO ClientConfig
readConfig basedir = do
  sessionId <- readFile $ basedir </> "session"
  return $ ClientConfig basedir sessionId

initConfig :: FilePath -> IO ()
initConfig basedir = do
  putStr "Enter session id: "
  hFlush stdout
  sessionId <- withEcho False getLine
  createDirectoryIfMissing True basedir
  writeFile (basedir </> "session") sessionId

initOrReadConfig :: Maybe FilePath -> IO ClientConfig
initOrReadConfig p = do
  basedir <- getBasedir p
  handleIOError (const $ initConfig basedir >> readConfig basedir) (readConfig basedir)

createRequest :: String -> Int -> Int -> IO Request
createRequest sessionId year day = do
  let url = printf "https://adventofcode.com/%d/day/%d/input" year day
  req <- parseRequest url
  let cookie = "session=" ++ sessionId
  return $ req {requestHeaders = [("Cookie", fromString cookie)]}

getLocalInputPath :: FilePath -> Int -> Int -> IO FilePath
getLocalInputPath basedir year day = do
  let dirname = basedir </> "input" </> show year
  createDirectoryIfMissing True dirname
  return $ dirname </> printf "day%02d.txt" day

downloadInput :: ClientConfig -> Int -> Int -> IO Text
downloadInput ClientConfig {sessionId, basedir} year day = do
  manager <- newManager tlsManagerSettings
  path <- getLocalInputPath basedir year day
  exists <- doesFileExist path
  if exists
    then do
      putStrLn $ "Reading input data from " ++ path
      IO.readFile path
    else do
      req <- createRequest sessionId year day
      resp <- httpLbs req manager
      let Status {statusCode} = responseStatus resp
      if statusCode == 200
        then do
          let body = toStrict $ decodeUtf8 $ responseBody resp
          putStrLn $ "Saving input data to " ++ path
          IO.writeFile path body
          return body
        else fail $ printf "Failed to download input data, status code %d" statusCode
