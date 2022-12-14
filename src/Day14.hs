module Day14 where

import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map

data Space = Empty | Wall | Sand
    deriving (Eq)
    
instance Show Space where
    show Empty = "."
    show Wall = "#"
    show Sand = "o"

data CaveSystem = Cave (Map (Integer, Integer) Space) Integer (Integer, Integer) (Integer, Integer)

getMap :: CaveSystem -> Map (Integer, Integer) Space
getMap (Cave m _ _ _) = m

instance Show CaveSystem where
    show (Cave m _ (ox,oy) (sx,sy)) = unlines [concat [Map.findWithDefault "." (x,y) (Map.insert (sx,sy) "+" $ Map.map show m) | x <- [minimum xs..maximum xs]] | y <- [minimum ys..maximum ys]]
        where
            ps = Map.keys m
            xs = ox : sx : map fst ps
            ys = oy : sy : map snd ps

stepCaveSystem :: CaveSystem -> Maybe CaveSystem
stepCaveSystem (Cave m my o (x,y)) = if y == my || m Map.!? o == Just Sand then Nothing else Just $ if not (null ps) then Cave m my o (head ps) else Cave (Map.insert (x,y) Sand m) my o o
    where
        ps = filter ((==Empty) . flip (Map.findWithDefault Empty) m) [(x,y+1), (x-1,y+1), (x+1,y+1)]

addWall :: CaveSystem -> ((Integer,Integer), (Integer, Integer)) -> CaveSystem
addWall (Cave m my o s) ((x1,y1),(x2,y2)) = Cave (foldl (flip (`Map.insert` Wall)) m ws) (maximum (my:map snd ws)) o s
    where
        ws = [(x,y) | x <- [min x1 x2..max x1 x2], y <- [min y1 y2..max y1 y2]] -- This assumes x1 == x2 || y1 == y2

parseLine :: String -> [((Integer, Integer), (Integer, Integer))]
parseLine = pairUp . getCorners
 where
     getCorners = map ((\[x, y] -> (read x, read y)) . splitOn ",") . splitOn " -> "
     pairUp ys = zip ys (tail ys)

parse :: String -> CaveSystem
parse = foldl addWall (Cave Map.empty 0 (500,0) (500,0)) . concatMap parseLine . lines

findEquilibrium :: CaveSystem -> CaveSystem
findEquilibrium cs = maybe cs findEquilibrium (stepCaveSystem cs)

partA :: CaveSystem -> Int
partA = length . filter (==Sand) . Map.elems . getMap . findEquilibrium

addFloor :: CaveSystem -> CaveSystem
addFloor c@(Cave _ my (sx,sy) _) = addWall c ((sx-sx-2,my+2),(sx + sx + 2,my+2))

partB :: CaveSystem -> Int
partB = length . filter (==Sand) . Map.elems . getMap . findEquilibrium . addFloor