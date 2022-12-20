module Day20 where

import Control.Arrow (first)
import Data.List
import Data.Array (Array, listArray)
import qualified Data.Array.IArray as Array

shift :: Int -> Index -> Value -> Index -> Index
shift ln (I j) (V d) (I i)
    | i == j = I j'
    | j < j' && j <= i && i <= j' = I (i - 1)
    | j' < j && j' <= i && i <= j = I (i + 1)
    | otherwise = I i
    where
        j'
            | d < 0 && (j + d) `mod` ln == 0 = ln - 1
            | j + d >= ln = (j + d) `mod` (ln - 1)
            | j + d < 0 = (j + d) `mod` (ln - 1) 
            | otherwise = j + d

newtype Index = I Int
    deriving (Eq, Ord, Show)
newtype Value = V Int
    deriving (Eq, Ord, Show)

mixStep :: Array Int (Index, Value) -> Int -> Index -> Value -> Array Int (Index, Value)
mixStep m ln j d = Array.amap (first (shift ln j d)) m

mix :: [Value] -> [Value]
mix xs = go xs (listArray (0,ln-1) $ zip (map I [0..]) xs)
    where
        ln = length xs
        go [] m = map snd . sortBy (\x y -> compare (fst x) (fst y)) $ Array.elems m
        go (d:ds) m = go ds (mixStep m ln (fst . head . filter ((==d).snd) $ Array.elems m) d)


parse :: String -> [Value]
parse = map (V . read) . lines

partA :: [Value] -> Int
partA xs = sum [v | a <- [1000,2000,3000], let V v = ms !! ((ix + a) `mod` ln)]
    where
        ln = length xs
        ms = mix xs
        (Just ix) = elemIndex (V 0) ms

partB = const 0
