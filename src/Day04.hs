module Day04 where

data Range = Range Integer Integer
    deriving (Show)

instance Read Range where
    readsPrec _ inp = [(Range x z, zs) | (x, y:ys) <- reads inp, y == '-', (z,zs) <- reads ys]

completeOverlap :: Range -> Range -> Bool
completeOverlap (Range x1 y1) (Range x2 y2) = (x1 <= x2 && y1 >= y2) || (x2 <= x1 && y2 >= y1)

anyOverlap :: Range -> Range -> Bool
anyOverlap (Range x1 y1) (Range x2 y2) = not $ null [x | x <- [x1..y1], x >= x2, x <= y2]

partA :: [(Range, Range)] -> Int
partA = length . filter (uncurry completeOverlap)

partB :: [(Range, Range)] -> Int
partB = length . filter (uncurry anyOverlap)

parseLine :: String -> (Range, Range)
parseLine inp = (r1, read ys)
    where
        [(r1,',':ys)] = reads inp

parse :: String -> [(Range, Range)]
parse = map parseLine . lines
