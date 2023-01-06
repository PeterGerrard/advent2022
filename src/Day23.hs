module Day23 where

import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.IO.Device (IODevice (getSize))

newtype Pos = Pos (Int, Int)
  deriving (Ord, Eq, Show)

getX :: Pos -> Int
getX (Pos (x, _)) = x

getY :: Pos -> Int
getY (Pos (_, y)) = y

data Direction = North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest
  deriving (Eq, Show)

maybeHead :: [a] -> Maybe a
maybeHead xs = if null xs then Nothing else Just (head xs)

move :: Direction -> Pos -> Pos
move North (Pos (x, y)) = Pos (x, y - 1)
move NorthEast (Pos (x, y)) = Pos (x + 1, y - 1)
move East (Pos (x, y)) = Pos (x + 1, y)
move SouthEast (Pos (x, y)) = Pos (x + 1, y + 1)
move South (Pos (x, y)) = Pos (x, y + 1)
move SouthWest (Pos (x, y)) = Pos (x - 1, y + 1)
move West (Pos (x, y)) = Pos (x - 1, y)
move NorthWest (Pos (x, y)) = Pos (x - 1, y - 1)

toCheck :: Direction -> [Direction]
toCheck North = [North, NorthEast, NorthWest]
toCheck South = [South, SouthEast, SouthWest]
toCheck East = [East, NorthEast, SouthEast]
toCheck West = [West, NorthWest, SouthWest]

dirsToCheck :: Direction -> [Direction]
dirsToCheck d = take 4 (dropWhile (/= d) ds ++ ds)
  where
    ds = [North, South, West, East]

step' :: Direction -> Set Pos -> Set Pos
step' d ps = Set.map doMove ps'
  where
    getTarget p =
      fromMaybe p
        . maybeHead
        $ [p | all (\d' -> Set.notMember (move d' p) ps) [North, NorthEast, East, SouthEast, South, SouthWest, West, NorthWest]]
          ++ [move d' p | d' <- dirsToCheck d, all (\d'' -> Set.notMember (move d'' p) ps) (toCheck d')]
    ps' = Set.map (\p -> (p, getTarget p)) ps
    doMove (f, t) = if null (Set.filter (\(f', t') -> f /= f' && t == t') ps') then t else f

getRect :: Set Pos -> ((Int, Int), (Int, Int))
getRect ps = ((Set.findMin xs, Set.findMin ys), (Set.findMax xs, Set.findMax ys))
  where
    xs = Set.map getX ps
    ys = Set.map getY ps

rectSize :: ((Int, Int), (Int, Int)) -> Int
rectSize ((a, b), (x, y)) = (x - a + 1) * (y - b + 1)

draw' :: Set Pos -> ((Int, Int), (Int, Int)) -> String
draw' ps ((minX, minY), (maxX, maxY)) = unlines [[if Set.member (Pos (x, y)) ps then '#' else '.' | x <- [minX .. maxX]] | y <- [minY .. maxY]]

draw :: Set Pos -> String
draw ps = draw' ps (getRect ps)

step :: (Set Pos, Direction) -> (Set Pos, Direction)
step (ps, d) = (step' d ps, head . tail $ dirsToCheck d)

iterateN :: Int -> (a -> a) -> a -> a
iterateN 0 _ s = s
iterateN n f s = iterateN (n - 1) f (f s)

parse :: String -> Set Pos
parse = Set.fromList . catMaybes . concat . zipWith (\y -> zipWith (\x c -> if c == '#' then Just (Pos (x, y)) else Nothing) [0 ..]) [0 ..] . lines

partA :: Set Pos -> Int
partA ps = (\r -> r - Set.size ps) . rectSize . getRect . fst $ iterateN 10 step (ps, North)

fixedPoint :: Eq a => (a -> a) -> a -> Int
fixedPoint = go 1
  where
    go acc f s = if s' == s then acc else go (acc + 1) f s'
      where
        s' = f s

partB :: Set Pos -> Int
partB ps = fixedPoint step (ps, North)