module Day03 where

import Data.Char (ord, isUpper)
import Data.List.Split (chunksOf)
import qualified Data.Set as Set

priority :: Char -> Int
priority c = ord c - if isUpper c then ord 'A' - 27 else ord 'a' - 1

overlap :: Ord a => ([a], [a]) -> [a]
overlap (xs,ys) = Set.toList $ Set.intersection (Set.fromList xs) (Set.fromList ys)

overlap3 :: Ord a => [[a]] -> [a]
overlap3 [xs,ys,zs] = overlap (xs, overlap (ys,zs))

partA :: [([Char], [Char])] -> Int
partA = sum . map (sum . map priority . overlap)

partB :: [([Char], [Char])] -> Int
partB = sum . map (sum . map priority . overlap3 . map (uncurry (++))) . chunksOf 3

parse :: String -> [([Char], [Char])]
parse = map parse' . lines

parse' :: String -> ([Char], [Char])
parse' inp = splitAt (l `div` 2) inp
    where
        l = length inp