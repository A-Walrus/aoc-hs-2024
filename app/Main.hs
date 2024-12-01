{-# LANGUAGE NamedFieldPuns #-}

import Base
import Data.Maybe
import qualified Solutions.Day01 as Day01
import System.Environment
import Text.Printf

days :: [String -> IO ()]
days =
  [ Day01.solution
  ]

data Args
  = RunDay
      { day :: Int,
        path :: Maybe String
      }
  | All

parseArgs :: [String] -> Args
parseArgs [] = All
parseArgs ["all"] = All
parseArgs [day] = RunDay {day = read day, path = Nothing}
parseArgs [day, path] = RunDay {day = read day, path = Just path}
parseArgs _ = error "Not enough args"

main :: IO ()
main = do
  a <- getArgs
  let args = parseArgs a
  runArgs args

runArgs :: Args -> IO ()
runArgs RunDay {day, path} = runDay day path
runArgs All = mapM_ (`runDay` Nothing) [1..length days]

runDay :: Int -> Maybe String -> IO ()
runDay day path = do
  printf " -- AOC Day %d -- \n" day
  let path' = fromMaybe "input" path
  let solution = days !! (day - 1)
  let filePath = printf "inputs/%02d/%s" day path'
  contents <- readFile filePath
  solution contents
  putStrLn ""
