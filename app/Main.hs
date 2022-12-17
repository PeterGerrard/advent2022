module Main where

import Day17 (parse, partA, partB)
import Control.Arrow

main :: IO ()
main = interact $ show . (partA &&& partB) . parse
