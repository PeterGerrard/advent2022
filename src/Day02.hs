module Day02 (parse, partA, partB) where

import Data.Bifunctor

data Move = Rock | Paper | Scissors
    deriving (Show, Eq)

data OppMove = A | B | C
    deriving (Read, Show)

data Response = X | Y | Z
    deriving (Read, Show)

data Strategy = Strategy OppMove Response
    deriving (Show)

data Game = Game Move Move
    deriving (Show)

winningMoves = [(Rock, Scissors),(Scissors, Paper),(Paper, Rock)]

scoreMove :: Move -> Integer
scoreMove Rock = 1
scoreMove Paper = 2
scoreMove Scissors = 3

scoreOutcome :: Move -> Move -> Integer
scoreOutcome x y
    | (x,y) `elem` winningMoves = 6
    | x == y = 3
    | otherwise = 0

score :: Game -> Integer
score (Game a x) = scoreMove x + scoreOutcome x a

oppMove :: OppMove -> Move
oppMove A = Rock
oppMove B = Paper
oppMove C = Scissors

calculateScore :: (Strategy -> Game) -> [Strategy] -> Integer
calculateScore f = sum . map (score.f)

partA :: [Strategy] -> Integer
partA = calculateScore applyStrat
    where
        applyStrat (Strategy a x) = Game (oppMove a) (myMove x) 
        myMove X = Rock
        myMove Y = Paper
        myMove Z = Scissors

partB :: [Strategy] -> Integer
partB = calculateScore applyStrat
    where
        applyStrat (Strategy a x) = Game a' (myMove x)
            where
                a' = oppMove a
                myMove X = head [z | (y,z) <- winningMoves, y == a']
                myMove Y = a'
                myMove Z = head [y | (y,z) <- winningMoves, z == a']

parse :: String -> [Strategy]
parse = map parseStrategy . lines

parseStrategy :: String -> Strategy
parseStrategy [a,' ',x] = Strategy (read [a]) (read [x])