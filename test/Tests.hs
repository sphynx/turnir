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

import RoundRobin
import qualified PairingEngine as PE

main = defaultMain tests

tests = [ testGroup "Round Robin"
          [ testProperty "Number of RR rounds" propRoundsNumber
          , testProperty "Number of RR games " propGamesNumber
          ]
        ]
-- generate n players
players n = map (\i -> PE.Player i ('P' : show i) 1800 PE.Available) [1 .. n]

-- number of rounds got from pairings list
rounds = length . makePairingsForAllRounds . players

-- number of games got from pairings list
games = sum . map (\(PE.RoundPairings _ gs _) -> length gs) . makePairingsForAllRounds . players

-- rounds number is equal to 1) N if N is even 2) N - 1 if N is odd -- due to byes
propRoundsNumber = forAll (choose (1, 10)) $ \n -> rounds n == (if even n then n - 1 else n)

-- games number is equal to N * (N-1) / 2
propGamesNumber = forAll (choose (1, 10)) $ \n -> games n == (n * (n - 1)) `div` 2
