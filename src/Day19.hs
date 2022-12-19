module Day19 where

import Algorithm.Search (aStar)
import Control.Arrow
import Data.List
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set

newtype Ore = O (Int, Int, Int, Int)
    deriving (Show, Eq, Ord)
newtype Robots = B (Int, Int, Int, Int)
    deriving (Show, Eq, Ord)

data State = S Int Robots Ore (Set Rule)
    deriving (Show, Eq, Ord)

data Rule = R Robots Ore
    deriving (Show, Eq, Ord)

canApply :: Rule -> State -> Bool
canApply (R _ (O (a,b,c,d))) (S _ _ (O (a',b',c',d')) _) = a <= a' && b <= b' && c <= c' && d <= d'

applyRule :: Rule -> State -> State
applyRule (R (B (a,b,c,d)) (O (x,y,z,w))) (S t (B (a',b',c',d')) (O (x',y',z',w')) rs) = S t (B (a'+a,b'+b,c'+c,d'+d)) (O (x'-x,y'-y,z'-z,w'-w)) Set.empty

getGR :: State -> Int
getGR (S _ (B (_,_,_,g)) _ _) = g

getG :: State -> Int
getG (S _ _ (O (_,_,_,g)) _) = g

heuristicTrim :: [State] -> [State]
heuristicTrim xs
    | null xs = []
    | getGR (last xs) > getGR (head xs) = [last xs]
    | length xs == 5 = tail xs
    | otherwise = xs

getNextStates :: Int -> Set Rule -> State -> [State]
getNextStates timeLimit rules s@(S t (B (a,b,c,d)) _ rs) = map update . heuristicTrim $ doNothing s:map (`applyRule` s) (Set.toList applicable)
    where
        update (S t r' (O (x,y,z,w)) rs) = S (t+1) r' (O (a+x,b+y,c+z,d+w)) rs
        doNothing (S t r o rs) = S t r o (Set.union rs applicable)
        applicable = Set.filter (\r -> r `Set.notMember` rs && canApply r s) rules

getCost :: Int -> State -> State -> Int
getCost timeLimit (S _ (B (_,_,_,g)) _ _) _ = timeLimit - g

predictCost :: Int -> State -> Int
predictCost timeLimit (S t (B (_,_,_,g)) _ _) = sum . take (timeLimit-t) $ iterate (\x -> x-timeLimit) (timeLimit-g)

score :: Int -> Set Rule -> Int
score timeLimit rs = getG . last . snd . fromJust $ aStar (getNextStates timeLimit rs) (getCost timeLimit) (predictCost timeLimit) (\(S t _ _ _) -> t==timeLimit) initialState

parseBlueprint :: String -> (Int, Set Rule)
parseBlueprint = toBlu . splitOn " "
    where
        toBlu xs = (read (init (xs !! 1)), Set.fromList [R (B (1,0,0,0)) (O (read (xs !! 6),0,0,0)),R (B (0,1,0,0)) (O (read (xs !! 12),0,0,0)),R (B (0,0,1,0)) (O (read (xs !! 18),read (xs !! 21),0,0)),R (B (0,0,0,1)) (O (read (xs !! 27),0,read (xs !!30),0))])

initialState :: State
initialState = S 0 (B (1,0,0,0)) (O (0,0,0,0)) Set.empty

parse :: String -> [(Int, Set Rule)]
parse = map parseBlueprint . lines

partA :: [(Int, Set Rule)] -> Int
partA = sum . map (uncurry (*) . second (score 24))

partB = product . map (score 32 . snd) . take 3