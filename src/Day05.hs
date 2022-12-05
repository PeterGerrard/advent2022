module Day05 where

import Data.List.Split
import Data.List
import Data.Maybe
import Data.Ord
import Data.Array (Array)
import qualified Data.Array as Array

partA ::  (Array Int [Char], [(Int, Int, Int)]) -> String
partA = map head . Array.elems . uncurry (foldl applyMove)

partB ::  (Array Int [Char], [(Int, Int, Int)]) -> String
partB = map head . Array.elems . uncurry (foldl applyMoveB)

parse :: String -> (Array Int [Char], [(Int, Int, Int)])
parse inp = (Array.listArray (1,9) . map catMaybes . transpose $ parseTowers p1, map parseMove p2)
    where
        [p1,_:p2] = splitWhen (isPrefixOf " 1") (lines inp)

parseTowers :: [String] -> [[Maybe Char]]
parseTowers = map (map parseTower . chunksOf 4)

parseTower :: String -> Maybe Char
parseTower (a:b:_) = if a == '[' then Just b else Nothing

parseMove :: String -> (Int, Int, Int)
parseMove inp = (read a, read f, read t)
    where
        [_,a,_,f,_,t] = splitOn " " inp

applyMove :: Array Int [Char] -> (Int, Int, Int) -> Array Int [Char]
applyMove towers (a,f,t) = towers Array.// [(f, drop a (towers Array.! f)), (t, reverse (take a (towers Array.! f)) ++ towers Array.! t)]

applyMoveB :: Array Int [Char] -> (Int, Int, Int) -> Array Int [Char]
applyMoveB towers (a,f,t) = towers Array.// [(f, drop a (towers Array.! f)), (t, take a (towers Array.! f) ++ towers Array.! t)]