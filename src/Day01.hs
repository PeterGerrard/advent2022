module Day01 where

import Data.List.Split
import Data.List
import Data.Ord

splitEmpty = filter (/=[""]) . split (dropBlanks . condense $ whenElt (==""))

partA :: [[Integer]] -> Integer
partA = maximum . map sum

sortDesc = sortOn Down

partB :: [[Integer]] -> Integer
partB = sum . take 3 . sortDesc . map sum

main = interact $ show . (\x -> (partA x, partB x)) . map (map read) . splitEmpty . lines