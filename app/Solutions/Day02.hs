module Solutions.Day02 (solution, part1, part2, parse) where

import Base
import Data.List
import Debug.Trace (trace)
import GHC.Base (assert)

solution :: String -> IO ()
solution = run (Day parse part1 part2)

type Parsed = [[Int]]

parse :: String -> Parsed
parse = map (map read . words) . lines

gradDec x = (x <= 3) && (x >= 1)

gradInc x = (x >= -3) && (x <= -1)

part1 :: Parsed -> Int
part1 = length . filter safe

safe l = all gradInc (diffs l) || all gradDec (diffs l)

diffs = zipWithNext (-)

part2 :: Parsed -> Int
part2 = length . filter safeish
  where
    safeish l = safe l || any ((\(a, b : rest) -> safe (a ++ rest)) . (`splitAt` l)) [0 .. (length l - 1)]
