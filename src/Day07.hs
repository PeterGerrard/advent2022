module Day07 where

import Data.List

data File = File String Integer
    deriving (Show)
data Directory = Directory String [Directory] [File] 
    deriving (Show)

data Line = CDUP | CD String | LS | DIR String | FILE Integer String
    deriving (Show)

parseLine :: String -> Line
parseLine x
    | x == "$ cd .." = CDUP
    | "$ cd " `isPrefixOf` x = CD (drop 5 x)
    | x == "$ ls" = LS
    | "dir" `isPrefixOf` x = DIR (drop 4 x)
    | otherwise = FILE a (tail b)
        where
            [(a,b)] = reads x

applyLines :: Directory -> [Line] -> (Directory, [Line])
applyLines d [] = (d,[])
applyLines d (CDUP:ls) = (d,ls)
applyLines (Directory path ds fs) (CD p:ls) = applyLines (Directory path (d':ds) fs) ls'
    where
        (d',ls') = applyLines (Directory p [] []) ls
applyLines (Directory path ds fs) (LS:ls) = applyLines (Directory path ds fs) ls
applyLines (Directory path ds fs) (DIR _:ls) = applyLines (Directory path ds fs) ls
applyLines (Directory path ds fs) (FILE s n:ls) = applyLines (Directory path ds (File n s:fs)) ls

parse :: String -> Directory
parse = fst . applyLines (Directory "" [] []) . map parseLine . lines 

size :: Directory -> Integer
size (Directory d ds fs) = sum (map (\(File _ s) -> s) fs) + sum (map size ds)

getDirectories :: Directory -> [Directory]
getDirectories d@(Directory _ ds fs) = d: concatMap getDirectories ds

partA :: Directory -> Integer
partA = sum . filter (<=100000) . map size . getDirectories

partB :: Directory -> Integer
partB d = minimum . filter (>= required) . map size . getDirectories $ d
    where
        s = size d
        required = s - 40000000

