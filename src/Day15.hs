module Day15 where

import Data.List
import Data.List.Split
import Data.Maybe

data Sensor = Sensor (Integer, Integer) (Integer, Integer)
    deriving (Show)

parseSensor :: String -> Sensor
parseSensor inp = Sensor (ox, oy) (sx, sy)
    where
        [ox,oy,sx,sy] = map read . concatMap (map (last . splitOn "=" . last . splitOn " ") . splitOn ",") $ splitOn ":" inp

parse :: String -> [Sensor]
parse = map parseSensor . lines

getBlockedOnRow :: Integer -> Sensor -> Maybe (Integer, Integer)
getBlockedOnRow y (Sensor (ox,oy) (sx,sy)) = if l' <= r' then Just (l',r') else Nothing -- [x | x <-[(ox-o)..(ox + o)], x /= sx || y /= sy]
    where
        md = abs (sx - ox) + abs (sy - oy)
        d = abs (y - oy)
        o = md - d
        (l,r) = (ox-o,ox+o)
        l' = if l == sx then l + 1 else l
        r' = if r == sx then r - 1 else r

mergeRanges :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
mergeRanges (a,b) (x,y)
    | a >= x && a <= y = (x, max b y)
    | x >= a && x <= b = (a, max b y)
    | otherwise = error "unmergeable"

addRange :: [(Integer, Integer)] -> (Integer, Integer) -> [(Integer, Integer)]
addRange [] r = [r]
addRange (r@(a,b):rs) r1@(x,y)
    | b < x = r:addRange rs r1
    | y < a = r1:r:rs
    | otherwise = addRange rs (mergeRanges r r1)

partA :: [Sensor] -> Integer
partA = sum . map ((+1) . uncurry (flip (-))) . foldl addRange [] . mapMaybe (getBlockedOnRow 2000000)

toPos :: (Integer, [(Integer,Integer)]) -> [(Integer, Integer)]
toPos (y, xs) = map (\x -> (snd x + 1, y)) xs
