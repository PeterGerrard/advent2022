module Day08 where

import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad

type Map2D a = Map Integer (Map Integer a)

lookup2D :: Integer -> Integer -> Map2D a -> Maybe a
lookup2D x y = Map.lookup y <=< Map.lookup x

bounds2D :: Map2D a -> ((Integer,Integer),(Integer,Integer))
bounds2D m = ((fst $ Map.findMin m, fst . Map.findMin . snd $ Map.findMin m),(fst $ Map.findMax m, fst . Map.findMax . snd $ Map.findMax m))

parse :: String -> Map2D Integer
parse = Map.fromList . zip [0..] . map (Map.fromList . zip [0..] . map (read. (: []))) . lines

gtM :: Integer -> Maybe Integer -> Bool
gtM _ Nothing = True
gtM x (Just y) = x > y

partA :: Map2D Integer -> Int
partA = length . findVisible

partB :: Map2D Integer -> Integer
partB m = maximum [scenicScore x y m | x <- [minx..maxx], y <- [miny..maxy]]
    where
        ((minx,miny), (maxx,maxy)) = bounds2D m

findVisible :: Map2D Integer -> [(Integer, Integer)]
findVisible m = nub (lr ++ rl ++ tb ++ bt)
    where
        ((minx,miny), (maxx,maxy)) = bounds2D m
        lr = concat [go (minx,y) (1,0) Nothing [] | y <- [miny..maxy]]
        rl = concat [go (maxx,y) (-1,0) Nothing [] | y <- [miny..maxy]]
        tb = concat [go (x,miny) (0,1) Nothing [] | x <- [minx..maxx]]
        bt = concat [go (x,maxy) (0,-1) Nothing [] | x <- [minx..maxx]]
        go (x,y) (xo,yo) mh acc = case lookup2D x y m of
                                    Nothing -> acc
                                    Just h -> go (x+xo,y+yo) (xo,yo) mh' acc'
                                        where
                                            mh' = if gtM h mh then Just h else mh
                                            acc' = if gtM h mh then (x,y):acc else acc

scenicScore :: Integer -> Integer -> Map2D Integer -> Integer
scenicScore x y m = lr * rl * tb * bt
    where
        ((minx,miny), (maxx,maxy)) = bounds2D m
        h = (m Map.! x) Map.! y
        lr = go (x+1,y) (1,0) 0
        rl = go (x-1,y) (-1,0) 0
        tb = go (x,y+1) (0,1) 0
        bt = go (x,y-1) (0,-1) 0
        go (x,y) (xo,yo) acc = case lookup2D x y m of
                                    Nothing -> acc
                                    Just h' -> if h <= h' then acc + 1 else go (x+xo,y+yo) (xo,yo) (acc+1)