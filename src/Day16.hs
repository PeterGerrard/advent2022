module Day16 where

import Algorithm.Search
import Control.Arrow
import Data.List
import Data.List.Split
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe

parseValve :: String -> (String, Integer, [String])
parseValve inp = (v,read . init $ drop 5 r,map (filter (/=',')) vs)
    where
        (_:v:_:_:r:_:_:_:_:vs) = splitOn " " inp

getDistances :: [(String, Integer, [String])] -> Map (String,String) Integer
getDistances inp = Map.fromList [((a,b),toInteger $ length d) | a <- Map.keys m, b <- Map.keys m, let Just d = bfs (m Map.!) (==b) a]
    where
        m = Map.fromList $ map (\(a,_,b) -> (a,b)) inp

parse  :: String -> (Map (String,String) Integer, Map String Integer)
parse = (getDistances &&& (Map.fromList . filter ((>0) . snd) . map (\(a,b,_) -> (a,b)))) . map parseValve . lines

example = "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB\nValve BB has flow rate=13; tunnels lead to valves CC, AA\nValve CC has flow rate=2; tunnels lead to valves DD, BB\nValve DD has flow rate=20; tunnels lead to valves CC, AA, EE\nValve EE has flow rate=3; tunnels lead to valves FF, DD\nValve FF has flow rate=0; tunnels lead to valves EE, GG\nValve GG has flow rate=0; tunnels lead to valves FF, HH\nValve HH has flow rate=22; tunnel leads to valve GG\nValve II has flow rate=0; tunnels lead to valves AA, JJ\nValve JJ has flow rate=21; tunnel leads to valve II"
input = "Valve XC has flow rate=0; tunnels lead to valves YK, AM\nValve ME has flow rate=0; tunnels lead to valves UU, SX\nValve EP has flow rate=0; tunnels lead to valves YS, QU\nValve GR has flow rate=0; tunnels lead to valves QZ, OG\nValve FA has flow rate=0; tunnels lead to valves DB, DP\nValve UJ has flow rate=0; tunnels lead to valves XN, CH\nValve QU has flow rate=0; tunnels lead to valves EP, YK\nValve OX has flow rate=19; tunnels lead to valves RI, PV\nValve VI has flow rate=0; tunnels lead to valves WI, XN\nValve IQ has flow rate=0; tunnels lead to valves QL, OG\nValve XO has flow rate=0; tunnels lead to valves GU, UI\nValve IY has flow rate=0; tunnels lead to valves VC, NT\nValve YS has flow rate=24; tunnel leads to valve EP\nValve XN has flow rate=7; tunnels lead to valves DG, UJ, VD, VI, OU\nValve AM has flow rate=6; tunnels lead to valves KA, NC, XC, TP, SI\nValve IH has flow rate=8; tunnels lead to valves TW, CH, WY, EC\nValve ZR has flow rate=18; tunnel leads to valve RI\nValve FP has flow rate=14; tunnels lead to valves DP, UF\nValve KA has flow rate=0; tunnels lead to valves VC, AM\nValve NC has flow rate=0; tunnels lead to valves UI, AM\nValve EC has flow rate=0; tunnels lead to valves IH, GU\nValve DG has flow rate=0; tunnels lead to valves AA, XN\nValve RI has flow rate=0; tunnels lead to valves OX, ZR\nValve NJ has flow rate=0; tunnels lead to valves YK, TW\nValve OG has flow rate=12; tunnels lead to valves GR, WY, IQ, UE\nValve IB has flow rate=0; tunnels lead to valves VB, UU\nValve RP has flow rate=0; tunnels lead to valves UI, OU\nValve OU has flow rate=0; tunnels lead to valves XN, RP\nValve NT has flow rate=0; tunnels lead to valves IY, AA\nValve MN has flow rate=0; tunnels lead to valves LX, VC\nValve SI has flow rate=0; tunnels lead to valves AM, AA\nValve VB has flow rate=0; tunnels lead to valves KT, IB\nValve UI has flow rate=4; tunnels lead to valves YI, XO, LX, NC, RP\nValve DL has flow rate=0; tunnels lead to valves GU, UE\nValve CH has flow rate=0; tunnels lead to valves UJ, IH\nValve WI has flow rate=0; tunnels lead to valves VI, VC\nValve GU has flow rate=11; tunnels lead to valves EC, XO, DL, SX\nValve KT has flow rate=17; tunnels lead to valves PV, VB\nValve TW has flow rate=0; tunnels lead to valves IH, NJ\nValve UE has flow rate=0; tunnels lead to valves DL, OG\nValve PV has flow rate=0; tunnels lead to valves KT, OX\nValve DP has flow rate=0; tunnels lead to valves FP, FA\nValve TP has flow rate=0; tunnels lead to valves VD, AM\nValve YI has flow rate=0; tunnels lead to valves AA, UI\nValve LX has flow rate=0; tunnels lead to valves UI, MN\nValve QZ has flow rate=0; tunnels lead to valves GR, UU\nValve DB has flow rate=23; tunnel leads to valve FA\nValve SX has flow rate=0; tunnels lead to valves ME, GU\nValve QL has flow rate=0; tunnels lead to valves AA, IQ\nValve YK has flow rate=16; tunnels lead to valves NJ, XC, QU\nValve VC has flow rate=5; tunnels lead to valves UF, KA, WI, IY, MN\nValve VD has flow rate=0; tunnels lead to valves TP, XN\nValve WY has flow rate=0; tunnels lead to valves IH, OG\nValve AA has flow rate=0; tunnels lead to valves YI, DG, QL, NT, SI\nValve UF has flow rate=0; tunnels lead to valves VC, FP\nValve UU has flow rate=15; tunnels lead to valves QZ, IB, ME"

getPath :: String -> Integer -> Map (String, String) Integer -> Map String Integer -> [Integer]
getPath o t0 m = move ([],0) t0 o
    where
        move acc t p mrate = if null (Map.filter (>0) mrate) then [snd acc] else concat [open acc (t - m Map.! (p, p')) p' mrate | (p',r) <- Map.assocs mrate, r > 0]
        open (acc,r) t p mrate
            | t > 0 = move (p:acc, r + (t-1) * (mrate Map.! p)) (t-1) p (Map.delete p mrate)
            | otherwise = [r]

partA :: (Map (String,String) Integer, Map String Integer) -> Integer
partA (distance, fs) = maximum $ getPath "AA" 30 distance fs

partitions :: Ord a => Map a b -> [(Map a b,Map a b)]
partitions = go [] []  . Map.assocs
    where
        go xs ys [] = [(Map.fromList xs, Map.fromList ys)]
        go xs ys (z:zs) = go (z:xs) ys zs ++ go xs (z:ys) zs

getDoublePath :: (String,String) -> Integer -> Map (String, String) Integer -> Map String Integer -> [Integer]
getDoublePath (o1,o2) t0 m mrate = [relieved o1 mrate1 + relieved o2 mrate2 | (mrate1, mrate2) <- partitions mrate]
    where
        relieved o = maximum . getPath o t0 m

-- partB :: (Map (String,String) Integer, Map String Integer) -> Integer
partB (distance, fs) = maximum $ getDoublePath ("AA","AA") 26 distance fs
