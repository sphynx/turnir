-- Td-tool -- a tool for tournament management.
--
-- Author : Ivan N. Veselov
-- Created: 24-Aug-2010
--
-- Copyright (C) 2010 Ivan N. Veselov
--
-- License: BSD3
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
module RoundRobin(
    makePairingsForAllRounds -- ^ create pairings for all the rounds
) where

import PairingEngine
import Data.List

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
rotate (f, s) = ( head f : head s : (init . tail $ f),
                  (tail s) ++ [(last f)] )

-- makes ready for use round-robin pairings but only with player indexes
rr :: Int -> [[(Int, Int)]]
rr n = map (uncurry zip) . take rounds . iterate rotate . round1 $ n
    -- a number of rounds is equal to (N - 1) if N is even, and N otherwise (because if
    -- the number is odd then every player has to skip one round having a bye).
    where rounds = if even n then (n - 1) else n

-- makes pairings for one round with actual players using indexes provided
mkRoundPairings :: Int -> [Player] -> [(Int, Int)] -> RoundPairings
mkRoundPairings n ps pairs = RoundPairings n games byes
    where (bye, normal) = partition (\(x, y) -> x == byeMark || y == byeMark) pairs
          player x = ps !! (x - 1)
          byes = map (\(x, y) -> if x == 0 then (player y) else (player x)) bye
          games = map (\(x, y) -> Game 0 (player x) (player y) NotStarted) normal

-- actually makes all the pairings with players
makePairingsForAllRounds :: [Player] -> Pairings
makePairingsForAllRounds ps = map (\(no, pairs) -> mkRoundPairings no ps pairs) . zipWith (,) [1..] . rr $ n
    where n = length ps
