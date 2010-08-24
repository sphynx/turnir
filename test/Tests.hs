-- Td-tool -- a tool for tournament management.
--
-- Author : Ivan N. Veselov
-- Created: 24-Aug-2010
--
-- Copyright (C) 2010 Ivan N. Veselov
--
-- License: BSD3
--
-- Tests suite.
--

module Main where

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck (testProperty)

import Test.QuickCheck
import Test.HUnit

import Data.List
import qualified Data.Map as M

import RoundRobin
import qualified PairingEngine as PE

main = defaultMain tests

tests = [ testGroup "Round Robin"
          [ testProperty "Number of RR rounds" propRoundsNumber
          , testProperty "Number of RR games " propGamesNumber
          , testProperty "Number of games played as white " propWhiteGames
          ]
        ]
-- generate n players
players n = map (\i -> PE.Player i ('P' : show i) 1800 PE.Available) [1 .. n]

pairings = makePairingsForAllRounds . players

-- number of rounds got from pairings list
roundsNo = length . pairings

-- number of games got from pairings list
gamesNo = sum . map (\(PE.RoundPairings _ gs _) -> length gs) . pairings

games = concatMap PE.pGames . pairings

-- gives the number of games as white for each player, in the following form:
-- Map.fromList [(P1,5),(P2,2),(P3,2),(P4,2),(P5,2),(P6,2)]
whites = accumMapWith PE.white . games

--
-- | This function converts something like [7,2,2,2] in M.fromList [(7,1), (2,3)], showing
-- quantity of occurenses element in given list.
-- @f@ is a function to transform the list elements before grouping.
--
accumMapWith :: (Ord a, Eq a) => (b -> a) -> [b] -> M.Map a Int
accumMapWith f = foldl' accF M.empty
    where accF map x = M.insertWith' (+) (f x) 1 map

-- rounds number is equal to 1) N if N is even 2) N - 1 if N is odd -- due to byes
propRoundsNumber = forAll (choose (1, 10)) $ \n -> roundsNo n == (if even n then n - 1 else n)

-- games number is equal to N * (N-1) / 2
propGamesNumber = forAll (choose (1, 10)) $ \n -> gamesNo n == (n * (n - 1)) `div` 2

-- a number of games played as white should be fairly distributes
-- that is, each player should play play near half of games as white
propWhiteGames = forAll (choose (2, 16)) $ \x ->
   let pred = if odd x then \y -> y /= x `div` 2
                       else \y -> y /= x `div` 2 && y /= (x `div` 2) - 1
   in M.null $ M.filter pred $ whites x