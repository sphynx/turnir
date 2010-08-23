-- Td-tool -- a tool for tournament management.
--
-- Author : Ivan N. Veselov
-- Created: 23-Aug-2010
--
-- Copyright (C) 2010 Ivan N. Veselov
--
-- License: BSD

module Main where

import System.Time
import Data.List

data Player = Player { pName :: String
                     , pRating :: Int
                     , pPlace :: String
                     , pDescr :: String
                     } deriving Show

data Tourney = Tourney { tName :: String
                       , tPlayers :: [Player]
                       , tPlace :: String
                       , tDescr :: String
                       } deriving Show

data TournamentType = Swiss | RoundRobin

-- round number, list of pairs, list of players getting bye in this round
data RoundPairings = RoundPairings Int [(Player, Player)] [Player] deriving Show

type Pairings = [RoundPairings]

data Game = Game Player Player GameResult ClockTime

data GameResult = NotStarted | Win | Loss | Draw | Adjourned | Cancelled | ForfeitWin | ForfeitLoss

--
-- Round-robin (all-play-all) pairings implementation.
--
-- It's possible to use two approaches for RR pairings. First is the very straightforward:
-- just use random each round. But usually another approach is used.
-- Idea of the scheduling algorithm is the following:
-- Assign each player a number (let's call it ID). Then pair them in the first round in the following way:
--
-- Round 1. (1 plays 14, 2 plays 13, ... )
--  1  2  3  4  5  6  7
--  14 13 12 11 10 9  8
--
-- Then fix one player (we use number 1 in the implementation) and rotate all the other player clockwise:
--
-- Round 2. (1 plays 13, 14 plays 12, ... )
-- 1  14 2  3  4  5  6
-- 13 12 11 10 9  8  7
--
-- Then continue rotating to get the pairs for the next round and so on until all the rounds are scheduled.
--
-- We have an interesting case while having an odd number of participants. Then we should add one "dummy"
-- player for example with ID = 0. If someone is playing with dummy player -- he has a rest day,
-- this is called "bye".
--
-- For further details, please see Wikipedia: http://en.wikipedia.org/wiki/Round-robin_tournament
--

-- index used for "dummy player" and as a mark of bye
byeMark :: Int
byeMark = 0

-- prepares pairing for the first round. It consists from two rows (lists) as in the example above
round1 :: Int -> ([Int], [Int])
round1 n
    | even n    = ( [1 .. half],     [n, n - 1 .. (half + 1)] )
    | otherwise = ( [1 .. half + 1], byeMark : [n, n - 1 .. (half + 2)] )
                  where half = n `div` 2

-- implements clockwise rotation, based on two rows/lists representation.
-- example (1 remains in place, all the other indexes rotate clockwise):
-- 1 2 3               1 6 2
-- 6 5 4  converts to  5 4 3
rotate :: ([Int], [Int]) -> ([Int], [Int])
rotate (f, s) = ( head f : head s : (init . tail $ f), (tail s) ++ [(last f)] )

-- makes ready for use round-robin pairings but only with player indexes
rr :: Int -> [[(Int, Int)]]
rr n = map (uncurry zip) . take n . iterate rotate . round1 $ n

-- makes pairings for one round with actual players using indexes provided
mkRoundPairings :: Int -> [Player] -> [(Int, Int)] -> RoundPairings
mkRoundPairings n ps pairs = RoundPairings n pairings byes
    where (bye, normal) = partition (\(x, y) -> x == byeMark || y == byeMark) pairs
          player x = ps !! (x - 1)
          byes = map (\(x, y) -> if x == 0 then (player y) else (player x)) bye
          pairings = map (\(x, y) -> (player x, player y)) normal

-- actually makes all the pairings with players
roundRobin :: [Player] -> Pairings
roundRobin ps = map (mkRoundPairings 1 ps) . rr $ (length ps)

-- generates test players list
players :: Int -> [Player]
players n = map (\x -> Player ("P" ++ show x) 1800 "Kyiv" "") [1 .. n]

main = do
    putStrLn "Welcome, tournament director! Let's see pairings for Round-Robin 5:"
    putStrLn (show . roundRobin . players $ 5)


