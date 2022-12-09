module Day09 where

import Data.Ord
import Data.List
import Data.List.Split

solve :: Int -> [Move] -> Int
solve n = length . nub . go ((0,0),replicate n (0,0)) [(0,0)]
    where
        go _ acc [] = acc
        go x acc (m:ms) = go (p1,p2) (last p2:acc) ms
            where
                (p1,p2) = applyMove x m

partA :: [Move] -> Int
partA = solve 1

partB :: [Move] -> Int
partB = solve 9

parse :: String -> [Move]
parse = concatMap ((\[x,y] -> replicate (read y) (read x)) . splitOn " ") . lines

data Move = U | D | L | R
    deriving (Eq,Read,Show)

applyMove :: ((Integer, Integer), [(Integer, Integer)]) -> Move -> ((Integer, Integer), [(Integer, Integer)])
applyMove ((a,b),ts) m = (h', followList h' ts) 
    where
        h' = case m of
            U -> (a,b-1)
            D -> (a,b+1)
            L -> (a-1,b)
            R -> (a+1,b)

followList :: (Integer, Integer) -> [(Integer, Integer)] -> [(Integer, Integer)]
followList _ [] = []
followList f (t:ts) = t':followList t' ts
    where
        t' = follow f t

follow :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
follow (a,b) (x,y) = (x',y')
    where
        (dx,dy) = (a - x, b - y)
        (x',y') = if abs dx <= 1 && abs dy <= 1 then (x,y) else (x + clamp (-1,1) dx,y + clamp (-1,1) dy)