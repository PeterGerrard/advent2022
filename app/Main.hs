module Main where

import Day06 (parse, partA, partB)
import Control.Arrow

main :: IO ()
main = interact $ show . (partA &&& partB) . parse
