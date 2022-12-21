module Day17 where

import Data.Array (Array)
import qualified Data.Array as Array
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

newtype Pos = Pos (Int, Int)
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

rocks :: Array Int Rock
rocks = Array.listArray (0,4) [rock1, rock2, rock3, rock4, rock5]

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

getX :: Pos -> Int
getX (Pos (x,_)) = x
getY :: Pos -> Int
getY (Pos (_,y)) = y

removeSurronded :: Set Pos -> Set Pos
removeSurronded ps = Set.filter (not . surronded) ps
    where
        surronded p = all ((\p@(Pos (x,y)) -> x < 1 || x > 7 || y < 1 || p `Set.member` ps) . addPos p . Pos) [(0,1),(0,-1),(1,0),(-1,0)]

merge :: (Set Pos, Int) -> Set Pos -> (Set Pos, Int)
merge (a,f) b = if null completeRows then (combined, f) else (Set.map (addPos (Pos (0,-maxY))) $ Set.filter ((>maxY) . getY) combined, maxY + f)
    where
        combined = Set.union a b
        toCheck = Set.toList $ Set.map getY b
        completeRows = filter (\y -> all (`Set.member` combined) [Pos (x,y) | x <- [1..7]]) toCheck
        maxY = maximum completeRows


fall :: (Set Pos, Pos, Rock, Int) -> Either (Set Pos, Pos, Rock, Int) (Set Pos, Int)
fall (occupied, p, r, f) = if overlap occupied rockps || offscreen then Right ((occupied,f) `merge` Set.fromList (offsetRock p r)) else Left (occupied, p', r, f)
    where
        p' = descend p
        rockps = offsetRock p' r
        offscreen = any ((<1) . getY) rockps

wind :: (Set Pos, Pos, Rock, Int) -> Wind -> (Set Pos, Pos, Rock, Int)
wind (occupied, p, r, f) w = (occupied, if overlap occupied rockps || offscreen then p else p', r, f)
    where
        p' = shift p w
        rockps = offsetRock p' r
        offscreen = any ((\x -> x<=0 || x>=8) . getX) rockps

completeFall :: Array Int Wind -> (Set Pos, Pos, Rock, Int) -> Int -> (Set Pos, Int, Int)
completeFall ws = go
    where
        go inp w = case fall (wind inp (ws Array.! w)) of
                        Left out -> go out w'
                        Right (occ, f') -> (occ, w', f')
                where
                    w' = (w+1) `mod` length ws

step :: Array Int Rock -> Array Int Wind -> (Set Pos, Int, Int, Int) -> (Set Pos, Int, Int, Int)
step rs ws (occupied, r, w, f) = (occupied', (r+1)`mod` length rs, w', f')
    where 
        (occupied', w', f') = completeFall ws (occupied, start, rs Array.! r, f) w
        start = Pos (3, if null occupied then 4 else Set.findMax (Set.map getY occupied) + 4)

draw :: Set Pos -> IO ()
draw s = putStr . (++"\n+-------+\n") $ intercalate "\n" ['|':[if Pos (x,y) `Set.member` s then '#' else '.' | x <- [1..7]] ++ "|" | y <- [9,8..1]]

parse :: String -> Array Int Wind
parse = (\xs -> Array.listArray (0,length xs - 1) xs) . map (read . (:[]))

solve :: Int -> Array Int Wind -> Int
solve n ws = (\(x,_,_, f) -> f + if Set.null x then 0 else (Set.findMax . Set.map getY) x) $ iterate (step rocks ws) (Set.empty, 0, 0, 0) !! n

partA :: Array Int Wind -> Int
partA = solve 2022

findLoop :: Array Int Rock -> Array Int Wind -> ([Int], [Int], Int)
findLoop rs ws = go [] [] (Set.empty, 0, 0, 0)
    where
        go seen floors (p, r, w, f)
            | (p,r,w) `elem` seen = (map (`solve` ws) [0..i-1], map (`solve` ws) [i..j-1], solve j ws - solve i ws)
            | otherwise = go ((p,r,w):seen) (f:floors) (step rs ws (p,r,w,f))
            where
                (Just i) = (p,r,w) `elemIndex` reverse seen
                j = length seen

partB :: Array Int Wind -> Int
partB ws = if ix < length hs then hs !! ix else (hl !! r) + inc * d
    where
        (hs, hl, inc) = findLoop rocks ws
        ix = 1000000000000
        (d, r) = (ix - length hs) `divMod` length hl
