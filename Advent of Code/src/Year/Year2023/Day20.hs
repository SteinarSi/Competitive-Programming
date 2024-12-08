module Year.Year2023.Day20(Day20(Day20)) where

import           Data.Bifunctor  (first, second)
import           Data.List       (delete, find)
import           Data.List.Split (splitOn)
import           Data.Map        (Map, elems, fromList, insert, keys, lookup)
import           Data.Sequence   (Seq (..), (><))
import qualified Data.Sequence   as Seq
import           Meta            (AoC (..))
import           Prelude         hiding (lookup)
import           Utility.Misc    (bruh)

data Day20 = Day20
instance AoC Day20 State Integer where
    date _ = (20,2023)
    parse _ = parseCircuit . map preParse . lines
    part1 _ = solulu 1000
    part2 _ state@(State {modules}) = pulse 1 [] (keys indeg) state
        where Just (Conjunction _ indeg _) = find (elem "rx" . outdegree) (elems modules)
    testAnswerPart1 _ = 11687500
    testAnswerPart2 _ = -1 -- Need to rewrite the test framework

data Module = FlipFlop {
        output    :: Bool,
        outdegree :: [String]
    } | Conjunction {
        output    :: Bool,
        indegree  :: Map String Bool,
        outdegree :: [String]
    }
    deriving (Show, Eq)

data State = State {
        modules :: Map String Module,
        highs   :: Integer,
        lows    :: Integer,
        queue   :: Seq Signal,
        entries :: [String]
    }
    deriving (Show, Eq)

data Signal = Signal {
    kind :: Bool,
    from :: String,
    to   :: String
}
    deriving (Eq)

instance Show Signal where
    show (Signal True  from to) = from ++ " -high-> " ++ to
    show (Signal False from to) = from ++ " -low-> "  ++ to

solulu :: Integer -> State -> Integer
solulu 0 state = lows state * highs state
solulu n state = solulu (n-1) state'
    where (_, _, state') = process 0 [] [] (press state)

press :: State -> State
press state = state {
        queue = Seq.fromList $ map (Signal False "roadcaster") (entries state),
        lows = lows state + 1
    }

pulse :: Integer -> [Integer] -> [String] -> State -> Integer
pulse _ ret [] _ = foldr lcm 1 ret
pulse n ret horsemen state = pulse (n+1) ( map (const n) hits ++ ret) hmen stat
    where (hits, hmen, stat) = process (n+1) horsemen [] (press state)

process :: Integer -> [String] -> [String] -> State -> ([String], [String], State)
process _  horsemen hits state@(State {queue = Empty}) = (hits, horsemen, state)
process it horsemen hits state@(State {queue = sig@(Signal { from, kind, to }) :<| rest})
    | not kind && elem to horsemen && out = process it (delete to horsemen) (to : hits) $ send state { queue = rest } sig
    | otherwise = process it horsemen hits $ send state { queue = rest } sig
    where Just Conjunction {indegree} = lookup to (modules state)
          newmap = insert from kind indegree
          out = not (and (elems newmap))

countSignal :: Bool -> State -> State
countSignal True  state = state { highs = highs state + 1 }
countSignal False state = state { lows  = lows  state + 1 }

send :: State -> Signal -> State
send state@(State {modules,queue}) Signal {kind, from, to} =
    countSignal kind $ case lookup to modules of
        Nothing -> state
        Just c@(Conjunction {indegree, outdegree}) -> state {
                                       modules = insert to (c { output = out, indegree = newmap }) modules,
                                       queue   = queue >< Seq.fromList (map (\o -> Signal {kind = out, from = to, to = o}) outdegree)
                                   }
            where newmap = insert from kind indegree
                  out = not (and (elems newmap))
        Just f@(FlipFlop {output, outdegree})
            | kind         -> state
            | otherwise -> state {
                modules = insert to (f { output = not output }) modules,
                queue   = queue >< Seq.fromList (map (Signal (not output) to) outdegree)
            }

preParse :: String -> (String, [String])
preParse s = (name, splitOn ", " names)
    where (name:names:_) = splitOn " -> " s

parseCircuit :: [(String, [String])] -> State
parseCircuit xs = State {
            highs = 0,
            lows  = 0,
            queue = Empty,
            modules = fromList modules,
            entries = entries
        }
    where
        (entries, modules) = parseCircuit' xs

        parseCircuit' :: [(String, [String])] -> ([String], [(String,Module)])
        parseCircuit' [] = ([], [])
        parseCircuit' (("broadcaster",outdegree):ys) = first (outdegree++) (parseCircuit' ys)
        parseCircuit' (('%':name, outdegree):ys) = second ((name, FlipFlop {output = False, outdegree}) :) (parseCircuit' ys)
        parseCircuit' (('&':name, outdegree):ys) = let indegree = fromList $ map ((,False) . tail . fst) (filter (elem name . snd) xs)
                                                   in  second ((name, Conjunction {output = True, indegree, outdegree = outdegree}) :) (parseCircuit' ys)
        parseCircuit' x = bruh x
