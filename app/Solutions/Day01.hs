module Solutions.Day01 (solution, part1, part2, parse) where

import Base
import Data.Char (digitToInt, isDigit)
import Data.List
import Data.Maybe (fromMaybe)

solution :: String -> IO ()
solution = run (Day parse part1 part2)

type Parsed = [[Int]]

parse :: String -> Parsed
parse = transpose . map (map read . words) . lines

part1 :: Parsed -> Int
part1 [a, b] = sum $ map abs $ zipWith (-) (sort a) (sort b)

part2 :: Parsed -> Int
part2 [as,bs]= sum $ map (\a -> a * length (filter (==a) bs)) as
