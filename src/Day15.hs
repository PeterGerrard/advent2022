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

getBlockedOnRow :: Bool -> Integer -> Sensor -> Maybe (Integer, Integer)
getBlockedOnRow includeSignal y (Sensor (ox,oy) (sx,sy)) = if l' <= r' then Just (if includeSignal then (l,r) else (l',r')) else Nothing
    where
        md = abs (sx - ox) + abs (sy - oy)
        d = abs (y - oy)
        o = md - d
        (l,r) = (ox-o,ox+o)
        l' = if l == sx then l + 1 else l
        r' = if r == sx then r - 1 else r

mergeRanges :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
mergeRanges (a,b) (x,y) = (min a x, max b y)

addRange :: [(Integer, Integer)] -> (Integer, Integer) -> [(Integer, Integer)]
addRange [] r = [r]
addRange (r@(a,b):rs) r1@(x,y)
    | b + 1 < x = r:addRange rs r1
    | y + 1 < a = r1:r:rs
    | otherwise = addRange rs (mergeRanges r r1)

partA :: [Sensor] -> Integer
partA = sum . map ((+1) . uncurry (flip (-))) . foldl addRange [] . mapMaybe (getBlockedOnRow False 2000000)

toPos :: (Integer, [(Integer,Integer)]) -> [(Integer, Integer)]
toPos (y, xs) = map (\x -> (snd x + 1, y)) xs

partB ss = (\(x,y) -> x*4000000 + y) . head . filter inBounds . concatMap toPos . filter ((>1) . length . snd) $ map (\y -> (y, foldl addRange [] $ mapMaybe (getBlockedOnRow True y) ss)) [0..4000000]
    where
        inBounds (x,y) = x >= 0 && x <= 4000000 && y >= 0 && y <= 4000000