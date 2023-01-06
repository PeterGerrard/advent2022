module Main where

import Control.Arrow
import Day23 (parse, partA, partB)

main :: IO ()
main = interact $ show . (partA &&& partB) . parse
