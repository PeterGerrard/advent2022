module Day13 where

import Data.List
import Data.List.Split
import Data.Char
import Data.Maybe
import Control.Arrow

data Signal = Signal Integer | SignalList [Signal]
    deriving (Eq)

instance Ord Signal where
    compare (Signal x) (Signal y) = compare x y
    compare (Signal x) (SignalList y) = compare (SignalList [Signal x]) (SignalList y)
    compare (SignalList x) (Signal y) = compare (SignalList x) (SignalList [Signal y])
    compare (SignalList []) (SignalList []) = EQ
    compare (SignalList []) (SignalList ys) = LT
    compare (SignalList xs) (SignalList []) = GT
    compare (SignalList (x:xs)) (SignalList (y:ys)) = case compare x y of
                                                        LT -> LT
                                                        EQ -> compare (SignalList xs) (SignalList ys)
                                                        GT -> GT

parseSignal :: String -> (Signal, String)
parseSignal = first (Signal . read) . span isNumber

parseSignalList :: String -> (Signal, String)
parseSignalList = go []
    where
        go acc (']':rs) = (SignalList acc, rs)
        go acc (',':rs) = go acc rs
        go acc inp = let (s,rs) = parseS inp in
                        go (acc ++ [s]) rs

parseS :: String -> (Signal, String)
parseS ('[':xs) = parseSignalList xs
parseS xs = parseSignal xs

instance Read Signal where
    readsPrec _ xs = [parseS xs]

instance Show Signal where
    show (SignalList xs) = show xs
    show (Signal x) = show x

parse :: String -> [(Signal, Signal)]
parse = map ((\[x,y] -> (read x, read y)) . take 2) . chunksOf 3 . lines

partA :: [(Signal, Signal)] -> Integer
partA = sum . map fst . filter ((==LT) . snd) . zip [1..] . map (uncurry compare)

dividers :: [Signal]
dividers = [SignalList [SignalList [Signal 2]], SignalList [SignalList [Signal 6]]]

getDividerIndicies :: [Signal] -> [Int]
getDividerIndicies xs = map (+1) $ mapMaybe (`elemIndex` xs) dividers

partB :: [(Signal, Signal)] -> Int
partB = product . getDividerIndicies . sort . (++ dividers) . concatMap (\(a,b) -> [a,b])