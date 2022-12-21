module Day21 where

import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map

data Func = Add | Subtract | Divide | Multiply
data Monkey = Value Int | Calculate Func String String | Human
data Result = Const Int | Calc Func Result Result | Unknown

isConst :: Result -> Bool
isConst (Const _) = True
isConst _ = False

calculateMonkey :: Map String Monkey -> Monkey -> Result
calculateMonkey m = go
    where
        go (Value x) = Const x
        go (Calculate f m1 m2) = case go (m Map.! m1) of
                                    Const x1 -> case go (m Map.! m2) of
                                                    Const x2 -> Const (f' x1 x2)
                                                    x2 -> Calc f (Const x1) x2
                                    x1 -> Calc f x1 (go (m Map.! m2))
            where f' = case f of
                        Add -> (+)
                        Subtract -> (-)
                        Divide -> div
                        Multiply -> (*)
        go Human = Unknown

parseMonkey :: String -> (String, Monkey)
parseMonkey inp = (id, if length ms == 1 then Value (read $ head ms) else Calculate f m1 m3)
    where
        [id,m] = splitOn ": " inp
        ms = splitOn " " m
        [m1,m2,m3] = ms
        f = case m2 of
                "+" -> Add
                "-" -> Subtract
                "/" -> Divide
                "*" -> Multiply

parse :: String -> Map String Monkey
parse = Map.fromList . map parseMonkey . lines

partA :: Map String Monkey -> Int
partA m = fromConst $ calculateMonkey m (m Map.! "root")
    where
        fromConst (Const x) = x

printResult :: Result -> String
printResult (Const x) = show x
printResult Unknown = "x"
printResult (Calc f a b) = "(" ++ printResult a ++ f' ++ printResult b ++ ")"
            where f' = case f of
                        Add -> "+"
                        Subtract -> "-"
                        Divide -> "/"
                        Multiply -> "*"

simplify :: Result -> Int -> (Result, Int)
simplify (Calc Add (Const x) y) z = simplify y (z-x)
simplify (Calc Add x (Const y)) z = simplify x (z-y)
simplify (Calc Subtract (Const x) y) z = simplify y (x-z)
simplify (Calc Subtract x (Const y)) z = simplify x (z+y)
simplify (Calc Multiply (Const x) y) z = simplify y (z `div` x)
simplify (Calc Multiply x (Const y)) z = simplify x (z `div` y)
simplify (Calc Divide x (Const y)) z = simplify x (z * y)
simplify r i = (r, i)


partB m = printResult p1 ++ " == " ++ show p2
    where
        (Calculate _ m1 m2) = m Map.! "root"
        m' = Map.insert "humn" Human m
        r1 = calculateMonkey m' (m' Map.! m1)
        (Const r2) = calculateMonkey m' (m' Map.! m2)
        (p1,p2) = simplify r1 r2


example = "root: pppw + sjmn\ndbpl: 5\ncczh: sllz + lgvd\nzczc: 2\nptdq: humn - dvpt\ndvpt: 3\nlfqf: 4\nhumn: 5\nljgn: 2\nsjmn: drzm * dbpl\nsllz: 4\npppw: cczh / lfqf\nlgvd: ljgn * ptdq\ndrzm: hmdt - zczc\nhmdt: 32"
