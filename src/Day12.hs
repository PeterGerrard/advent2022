module Day12 where

import Algorithm.Search (bfs)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Char (ord)
import Data.Maybe (fromJust, catMaybes)

data Pos = Pos Integer Integer
    deriving (Eq, Show, Ord)
data HeightMapInfo = Start | End | Height Int
    deriving (Eq, Show)
type HeightMap = Map Pos HeightMapInfo

parseHeightMapInfo :: Char -> HeightMapInfo
parseHeightMapInfo 'S' = Start
parseHeightMapInfo 'E' = End
parseHeightMapInfo x = Height (ord x - ord 'a' + 1)

parseHeightMapRow :: Integer -> String -> [(Pos, HeightMapInfo)]
parseHeightMapRow y = zipWith (\x c -> (Pos x y, parseHeightMapInfo c)) [0..]

parseHeightMap :: String -> [(Pos, HeightMapInfo)]
parseHeightMap = concat . zipWith parseHeightMapRow [0..] . lines

parse :: String -> (HeightMap, Pos, Pos, [Pos])
parse inp = (Map.fromList hs, s, e, s:as)
    where
        hs = parseHeightMap inp
        s = head [p | (p,Start) <- hs]
        as = [p | (p,Height 1) <- hs]
        e = head [p | (p,End) <- hs]

getHeight :: HeightMapInfo -> Int
getHeight Start = 1
getHeight End = 26
getHeight (Height x) = x

getShortestRoute :: HeightMap -> Pos -> Pos -> Maybe Int
getShortestRoute m s e = length <$> bfs getAdjacent (==e) s
    where
        getAdjacent (Pos x y) = filter (climeable (m Map.! Pos x y) . (Map.!) m) $ filter (`Map.member` m) [Pos (x+1) y, Pos (x-1) y, Pos x (y+1), Pos x (y-1)]
        climeable hm1 hm2 = getHeight hm2 - getHeight hm1 <= 1

partA :: (HeightMap, Pos, Pos, [Pos]) -> Int
partA (m,s,e,_) = fromJust $ getShortestRoute m s e

partB :: (HeightMap, Pos, Pos, [Pos]) -> Int
partB (m,_,e,ss) = minimum $ catMaybes [getShortestRoute m s e | s <- ss]