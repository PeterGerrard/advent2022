module Day11 where

import Data.List
import Data.List.Split
import Data.Array
import Control.Arrow (second)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Ord

data Monkey = Monkey Integer [Integer] (Integer -> Integer) Integer Integer Integer

getId :: Monkey -> Integer
getId (Monkey id _ _ _ _ _) = id

countItems :: Monkey -> Int
countItems (Monkey _ xs _ _ _ _) = length xs

parseMonkeyId :: String -> Integer
parseMonkeyId = read . head . splitOn ":" . last . splitOn " "

parseStartingItems :: String -> [Integer]
parseStartingItems = map read . splitOn "," . last . splitOn ": "

parseOperation :: String -> (Integer -> Integer)
parseOperation = f . splitOn " " . last . splitOn "= "
    where
        f [x,o,y] old = (if o=="+" then (+) else (*)) (if x == "old" then old else read x) (if y == "old" then old else read y)

parseTest :: String -> Integer
parseTest = read . last . splitOn " "

parseThrow  :: String -> Integer
parseThrow = read . last . splitOn " "

parseMonkey :: String -> Monkey
parseMonkey inp = Monkey (parseMonkeyId l1) (parseStartingItems l2) (parseOperation l3) (parseTest l4) (parseThrow l5) (parseThrow l6)
    where
        [l1,l2,l3,l4,l5,l6] = lines inp

parse :: String -> Array Integer Monkey
parse = ta . map ((\m -> (getId m, m)) . parseMonkey) . splitOn "\n\n"
    where
        ta xs = array (minimum $ map fst xs, maximum $ map fst xs) xs

stepMonkey :: (Integer -> Integer) -> Monkey -> [(Integer, Integer)]
stepMonkey _ (Monkey _ [] _ _ _ _) = []
stepMonkey g (Monkey id (x:xs) f t a b) = (if test x' then a else b, x'):stepMonkey g (Monkey id xs f t a b)
    where
        test i = i `mod` t == 0
        x' = g (f x)

emptyHand :: Integer -> Array Integer Monkey -> Array Integer Monkey
emptyHand i a = accum (\(Monkey id _ a b c d) _ -> Monkey id [] a b c d) a [(i,i)]

receive :: Monkey -> Integer -> Monkey
receive (Monkey id xs a b c d) x = Monkey id (xs ++ [x]) a b c d

roundMonkey :: (Integer -> Integer) -> (Array Integer Monkey, Map Integer Int) -> (Array Integer Monkey, Map Integer Int)
roundMonkey g a = go n a
    where
        (n,m) = bounds (fst a)
        go i (a,cs) = if i > m then (a,cs) else go (i+1) (emptyHand i $ accum receive a moves, Map.alter (Just . maybe z (+z)) i cs)
            where
                mk = a ! i
                moves = stepMonkey g mk
                z = countItems mk

printMonkey :: Monkey -> String
printMonkey (Monkey id xs _ _ _ _) = show id ++ ": " ++ show xs

getClamp :: [Monkey] -> Integer
getClamp = foldl (\acc (Monkey _ _ _ t _ _) -> lcm acc t) 1

sortDesc = sortOn Down

solve :: Int -> (Integer -> Integer) -> Array Integer Monkey -> Int
solve x g a = product . take 2 . sortDesc . Map.elems . snd . (!! x) . iterate (roundMonkey ((`mod` clampValue) . g)) $ (a,Map.empty)
    where
        clampValue = getClamp $ elems a

partA :: Array Integer Monkey -> Int
partA = solve 20 (`div` 3)

partB :: Array Integer Monkey -> Int
partB = solve 10000 id