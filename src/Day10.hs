module Day10 where

import Data.List
import Data.List.Split

data Instruction = Noop | Addx Integer
    deriving (Show)

readNoop :: String -> [(Instruction, String)]
readNoop inp = [(Noop, drop 4 inp) | "noop" `isPrefixOf` inp]

readAddx :: String -> [(Instruction, String)]
readAddx inp = [(Addx n, rs) | "addx " `isPrefixOf` inp, (n, rs) <- reads (drop 5 inp)]

instance Read Instruction where
    readsPrec _ inp = concatMap (\f -> f inp) [readNoop, readAddx]

parse :: String -> [Instruction]
parse = map read . lines

applyInstruction :: Integer -> Instruction -> ([Integer], Integer)
applyInstruction x Noop = ([x],x)
applyInstruction x (Addx y) = ([x,x+y],x+y)

everyN :: Integer -> [a] -> [a]
everyN n = go n []
    where
        go _ acc [] = acc
        go 1 acc (x:xs) = go n (acc ++ [x]) xs
        go m acc (_:xs) = go (m-1) acc xs

everyOther :: [a] -> [a]
everyOther [] = []
everyOther (x:xs) = x:everyN 2 xs

registryValues :: [Instruction] -> [Integer]
registryValues = go [1] 1
    where
        go acc _ [] = acc
        go acc x (m:ms) = go (acc++cs) x' ms
            where
                (cs,x') = applyInstruction x m

partA :: [Instruction] -> Integer
partA = sum . zipWith (*) [20,60,100,140,180,220] . everyOther . everyN 20 . registryValues


partB :: [Instruction] -> String
partB = intercalate "\n" . chunksOf 40 . map prn . zipWith (-) (concat $ replicate 6 [0..39]) . registryValues
    where
        prn x = if abs x <= 1 then '#' else '.'