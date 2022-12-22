module Day22 where

import Prelude hiding (Left, Right)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

data Space = Empty | Wall
    deriving (Eq)
type Map2D = Map (Int, Int) Space
data Direction = Up | Right | Down | Left
    deriving (Show)
data Instruction = Move Int | TurnR | TurnL
    deriving (Show)
data State = State (Int,Int) Direction
    deriving (Show)

parseSpace :: Char -> Maybe Space
parseSpace '.' = Just Empty
parseSpace '#' = Just Wall
parseSpace _ = Nothing

raise :: (a, Maybe b) -> Maybe (a,b)
raise (x, Nothing) = Nothing
raise (x, Just y) = Just (x, y)

parseMap :: [String] -> Map2D
parseMap = Map.fromList . mapMaybe raise . concat . zipWith (\y -> zipWith (\x c -> ((x,y), parseSpace c)) [1..]) [1..]

getMinX :: Map2D -> Int -> Int
getMinX m y = minimum . map fst . filter ((==y) . snd) $ Map.keys m
getMaxX :: Map2D -> Int -> Int
getMaxX m y = maximum . map fst . filter ((==y) . snd) $ Map.keys m
getMinY :: Map2D -> Int -> Int
getMinY m x = minimum . map snd . filter ((==x) . fst) $ Map.keys m
getMaxY :: Map2D -> Int -> Int
getMaxY m x = maximum . map snd . filter ((==x) . fst) $ Map.keys m

getStart :: Map2D -> (Int, Int)
getStart m = (minimum xs, y)
    where
        y = 1
        xs = map fst . filter ((==y) . snd) $ Map.keys m

parseTurn :: Char -> Instruction
parseTurn 'R' = TurnR
parseTurn 'L' = TurnL

parseInstructions :: String -> [Instruction]
parseInstructions s
    | null s = []
    | null xs = parseTurn (head s):parseInstructions (tail s)
    | otherwise = Move m:parseInstructions s'
    where
        xs = reads s
        [(m,s')] = xs

left :: Direction -> Direction
left Up = Left
left Right = Up
left Down = Right
left Left = Down

right :: Direction -> Direction
right Up = Right
right Right = Down
right Down = Left
right Left = Up

adjWrap :: Map2D -> State -> State
adjWrap m (State (x,y) Up) = State (if Map.member (x, y-1) m then (x, y-1) else (x, getMaxY m x)) Up
adjWrap m (State (x,y) Down) = State (if Map.member (x, y+1) m then (x, y+1) else (x, getMinY m x)) Down
adjWrap m (State (x,y) Left) = State (if Map.member (x-1, y) m then (x-1, y) else (getMaxX m y, y)) Left
adjWrap m (State (x,y) Right) = State (if Map.member (x+1, y) m then (x+1, y) else (getMinX m y, y)) Right

move :: Map2D -> (State -> State) -> State -> Int -> State
move m _ s 0 = s
move m adj s@(State p d) n = if m Map.! p' == Empty then move m adj s' (n-1) else s
    where s'@(State p' _) = adj s

applyInstruction :: (State -> Int -> State) -> State -> Instruction -> State
applyInstruction _ (State p d) TurnL = State p (left d)
applyInstruction _ (State p d) TurnR = State p (right d)
applyInstruction mv s (Move n) = mv s n

parse :: String -> (Map2D, [Instruction])
parse inp = (parseMap ms, parseInstructions (last ls))
    where
        ls = lines inp
        ms = init (init ls)

score :: State -> Int
score (State (x,y) d) = 1000 * y + 4*x + facing 
    where
        facing = case d of
                    Right -> 0
                    Down -> 1
                    Left -> 2
                    Up -> 3

partA (m, is) = score $ foldl (applyInstruction (move m (adjWrap m))) (State (getStart m) Right) is

-- hardcoded to my input
adjCube :: State -> State
adjCube (State (50,y) Right)
    | y > 150 = State (y-100, 150) Up
    | otherwise = State (51,y) Right
adjCube (State (100,y) Right)
    | y <= 50 = State (101, y) Right
    | y <= 100 = State (y + 50, 50) Up
    | y <= 150 = State (150, 151 - y) Left
adjCube (State (150,y) Right) = State (100, 151 - y) Left
adjCube (State (x,y) Right) = State (x+1, y) Right
adjCube (State (1,y) Left)
    | y <= 150 = State (51, 151-y) Right
    | otherwise = State (y - 100, 1) Down
adjCube (State (51,y) Left)
    | y <= 50 = State (1, 151-y) Right
    | y <= 100 = State (y-50, 101) Down
    | otherwise = State (50, y) Left
adjCube (State (x,y) Left) = State (x-1,y) Left
adjCube (State (x,1) Up)
    | x <= 100 = State (1, 100+x) Right
    | otherwise = State (x - 100, 200) Up
adjCube (State (x,101) Up)
    | x <= 50 = State (51, 50+x) Right
    | otherwise = State (x, 100) Up
adjCube (State (x,y) Up) = State (x, y - 1) Up
adjCube (State (x,50) Down)
    | x > 100 = State (100, x - 50) Left
    | otherwise = State (x, 51) Down
adjCube (State (x,150) Down)
    | x > 50 = State (50, x +100) Left
    | otherwise = State (x, 151) Down
adjCube (State (x,200) Down) = State (x + 100, 1) Down
adjCube (State (x,y) Down) = State (x, y + 1) Down

partB (m, is) = score $ foldl (applyInstruction (move m adjCube)) (State (getStart m) Right) is