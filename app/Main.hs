module Main where

import Day01 (parse, partA, partB)
import Control.Arrow

main :: IO ()
main = interact $ show . (partA &&& partB) . parse
