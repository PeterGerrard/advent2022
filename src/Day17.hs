module Day17 where

import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

newtype Pos = Pos (Integer, Integer)
    deriving (Show, Ord, Eq)
newtype Rock = Rock [Pos]
    deriving (Show)

data Wind = LeftW | RightW

instance Show Wind where
    show LeftW = "<"
    show RightW = ">"
instance Read Wind where
    readsPrec _ ('<':rs) = [(LeftW, rs)]
    readsPrec _ ('>':rs) = [(RightW, rs)]
    readsPrec _ _ = []


rock1 = Rock $ map Pos [(0,0),(1,0),(2,0),(3,0)]
rock2 = Rock $ map Pos [(1,0),(0,1),(1,1),(2,1),(1,2)]
rock3 = Rock $ map Pos [(0,0),(1,0),(2,0),(2,1),(2,2)]
rock4 = Rock $ map Pos [(0,0),(0,1),(0,2),(0,3)]
rock5 = Rock $ map Pos [(0,0),(1,0),(0,1),(1,1)]

rocks :: [Rock]
rocks = concat $ repeat [rock1, rock2, rock3, rock4, rock5]

descend :: Pos -> Pos
descend (Pos (x,y)) = Pos (x, y-1)

shift :: Pos -> Wind -> Pos
shift (Pos (x,y)) LeftW = Pos (x-1, y)
shift (Pos (x,y)) RightW = Pos (x+1, y)

addPos :: Pos -> Pos -> Pos
addPos (Pos (x,y)) (Pos (a,b)) = Pos (x+a, y+b)

offsetRock :: Pos -> Rock -> [Pos]
offsetRock p (Rock rs) = map (addPos p) rs

overlap :: Set Pos -> [Pos] -> Bool
overlap s = any (`Set.member` s)

getX :: Pos -> Integer
getX (Pos (x,_)) = x
getY :: Pos -> Integer
getY (Pos (_,y)) = y

removeSurronded :: Set Pos -> Set Pos
removeSurronded ps = Set.filter (not . surronded) ps
    where
        surronded p = all ((\p@(Pos (x,y)) -> x < 1 || x > 7 || y < 1 || p `Set.member` ps) . addPos p . Pos) [(0,1),(0,-1),(1,0),(-1,0)]

merge :: Set Pos -> Set Pos -> Set Pos
merge a b = removeSurronded $ if null completeRows then combined else Set.filter ((>=maxY) . getY) combined
    where
        combined = Set.union a b
        toCheck = Set.toList $ Set.map getY b
        completeRows = filter (\y -> all (`Set.member` combined) [Pos (x,y) | x <- [1..7]]) toCheck
        maxY = maximum completeRows


fall :: (Set Pos, Pos, Rock) -> Either (Set Pos, Pos, Rock) (Set Pos)
fall (occupied, p, r) = if overlap occupied rockps || offscreen then Right (occupied `merge` Set.fromList (offsetRock p r)) else Left (occupied, p', r)
    where
        p' = descend p
        rockps = offsetRock p' r
        offscreen = any ((<1) . getY) rockps

wind :: (Set Pos, Pos, Rock) -> Wind -> (Set Pos, Pos, Rock)
wind (occupied, p, r) w = (occupied, if overlap occupied rockps || offscreen then p else p', r)
    where
        p' = shift p w
        rockps = offsetRock p' r
        offscreen = any ((\x -> x<=0 || x>=8) . getX) rockps

completeFall :: (Set Pos, Pos, Rock) -> [Wind] -> (Set Pos, [Wind])
completeFall inp (w:ws) = case fall (wind inp w) of
                            Left out -> completeFall out ws
                            Right occ -> (occ, ws)

step :: (Set Pos, [Rock], [Wind]) -> (Set Pos, [Rock], [Wind])
step (occupied, r:rs, ws) = (occupied', rs, ws')
    where 
        (occupied', ws') = completeFall (occupied, start, r) ws
        start = Pos (3, if null occupied then 4 else Set.findMax (Set.map getY occupied) + 4)

draw :: Set Pos -> IO ()
draw s = putStr . (++"\n+-------+\n") $ intercalate "\n" ['|':[if Pos (x,y) `Set.member` s then '#' else '.' | x <- [1..7]] ++ "|" | y <- [9,8..1]]

parse :: String -> [Wind]
parse = map (read . (:[]))

solve :: Int -> [Wind] -> Integer
solve n ws = Set.findMax . Set.map getY . (\(x,_,_) -> x) $ iterate step (Set.empty, rocks, concat (repeat ws)) !! n

partA :: [Wind] -> Integer
partA = solve 2022

partB :: [Wind] -> Integer
partB = solve 1000000000000