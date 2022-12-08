module Main where

import Day08 (parse, partA, partB)
import Control.Arrow

main :: IO ()
main = interact $ show . (partA &&& partB) . parse
