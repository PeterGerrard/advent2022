module Day02 where

import Data.Bifunctor

data Move = Rock | Paper | Scissors
    deriving (Show)

data OppMove = A | B | C
    deriving (Read, Show)

data Response = X | Y | Z
    deriving (Read, Show)

data Strategy = Strategy OppMove Response
    deriving (Show)

data Game = Game Move Move
    deriving (Show)

scoreMove :: Move -> Integer
scoreMove Rock = 1
scoreMove Paper = 2
scoreMove Scissors = 3

scoreOutcome :: Move -> Move -> Integer
scoreOutcome Rock Paper = 6
scoreOutcome Paper Scissors = 6
scoreOutcome Scissors Rock = 6
scoreOutcome Paper Rock = 0
scoreOutcome Scissors Paper = 0
scoreOutcome Rock Scissors = 0
scoreOutcome _ _ = 3

score :: Game -> Integer
score (Game a x) = scoreMove x + scoreOutcome a x

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
        applyStrat (Strategy a x) = Game (oppMove a) (myMove a x) 
        myMove A X = Scissors
        myMove A Y = Rock
        myMove A Z = Paper
        myMove B X = Rock
        myMove B Y = Paper
        myMove B Z = Scissors
        myMove C X = Paper
        myMove C Y = Scissors
        myMove C Z = Rock

parse :: String -> [Strategy]
parse = map parseStrategy . lines

parseStrategy :: String -> Strategy
parseStrategy [a,' ',x] = Strategy (read [a]) (read [x])