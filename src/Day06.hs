module Day06 where

import Data.List

solve :: Int -> String -> Int
solve n = go n
    where
        go acc xs = if length (nub (take n xs)) == n then acc else go (acc+1) (tail xs)

partA :: String -> Int
partA = solve 4

partB :: String -> Int
partB = solve 14

parse :: String -> String
parse = id
