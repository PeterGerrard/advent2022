module Day18 where

import Algorithm.Search (bfs)
import Data.Maybe (isJust)
import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as Set

newtype Pos3D = P (Int,Int,Int)
    deriving (Eq, Ord, Show)

getAdjacent :: Pos3D -> [Pos3D]
getAdjacent (P (x,y,z)) = [P (x,y,z-1),P (x,y,z+1),P (x,y-1,z),P (x,y+1,z),P (x-1,y,z),P (x+1,y,z)]

data Field = Field (Set Pos3D) Integer
    deriving (Show)

getSurfaceArea :: Field -> Integer
getSurfaceArea (Field _ sa) = sa

addBlock :: Field -> Pos3D -> Field
addBlock (Field ps sa) p= Field (Set.insert p ps) (6 + sa - 2* toInteger (length adj))
    where
        adj = filter (`Set.member` ps) (getAdjacent p)

parseLine :: String -> Pos3D
parseLine = toPos . splitOn ","
    where toPos [x,y,z] = P (read x, read y, read z)

parse :: String -> [Pos3D]
parse = map parseLine . lines

partA :: [Pos3D] -> Integer
partA = getSurfaceArea . foldl addBlock (Field Set.empty 0)

getOpenAirFaces :: (Pos3D -> Bool) -> Set Pos3D -> Pos3D -> Integer
getOpenAirFaces escaped ps p = toInteger (length escapePaths)
    where
        escapePaths = filter (isJust . bfs (filter (`Set.notMember` ps) . getAdjacent) escaped) (filter (`Set.notMember` ps) (getAdjacent p))

partB :: [Pos3D] -> Integer
partB ps = sum $ map (getOpenAirFaces (not . inBounds) sps) ps
    where
        sps = Set.fromList ps
        minx = Set.findMin (Set.map (\(P (x,_,_)) -> x) sps)
        maxx = Set.findMax (Set.map (\(P (x,_,_)) -> x) sps)
        miny = Set.findMin (Set.map (\(P (_,y,_)) -> y) sps)
        maxy = Set.findMax (Set.map (\(P (_,y,_)) -> y) sps)
        minz = Set.findMin (Set.map (\(P (_,_,z)) -> z) sps)
        maxz = Set.findMax (Set.map (\(P (_,_,z)) -> z) sps)
        inBounds (P (x,y,z)) = minx <= x && x <= maxx && miny <= y && y <= maxy && minz <= z && z <= maxz
