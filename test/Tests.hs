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

import RoundRobin
import qualified PairingEngine as PE

main = defaultMain tests

tests = [ testGroup "Round Robin"
          [ testCase "Number of rounds even" test_rr_rounds_no_1
          , testCase "Number of rounds odd" test_rr_rounds_no_2
          , testProperty "Number of rounds in general" prop_rounds_no
          , testProperty "Number of games in general" prop_games_no
          ]
        ]
-- generate n players
players n = map (\i -> PE.Player i ("P" ++ show i) 1800 PE.Available) [1 .. n]

-- number of rounds got from pairings list
rounds = length . makePairingsForAllRounds . players

-- number of games got from pairings list
games = sum . map (\(PE.RoundPairings _ gs _) -> length gs) . makePairingsForAllRounds . players

test_rr_rounds_no_1 = rounds 10 @?= 9
test_rr_rounds_no_2 = rounds 3 @?= 3

prop_rounds_no = forAll (choose (1, 10)) $ \n -> rounds n == (if even n then n - 1 else n)
prop_games_no = forAll (choose (1, 10)) $ \n -> games n == (n * (n - 1)) `div` 2
