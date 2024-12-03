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
part2 = length . filter slow
  where
    slow l = safe l || any ((\(a, b : rest) -> safe (a ++ rest)) . (`splitAt` l)) [0 .. (length l - 1)]

-- safish l@(x : _) = f gradInc (x - 1 : l) True || f gradDec (x + 1 : l) True

-- f :: (Int -> Bool) -> [Int] -> Bool -> Bool
-- f cond [] _ = True
-- f cond [_] _ = True
-- f cond [a, b, c] True = cond (a - b)
-- f cond (a : b : c : rest) True = (cond (a - b) && f cond (b : c : rest) True) || (cond (a - c) && f cond (c : rest) False)
-- f cond (a : b : rest) canUse = cond (a - b) && f cond (b : rest) canUse
