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
import PairingEngine    
    
main = defaultMain tests

tests = [ testGroup "Round Robin"
          [ testCase "Number of rounds even" test_rr_rounds_no_1
          , testCase "Number of rounds odd" test_rr_rounds_no_2
          ]
        ]
          
test_rr_rounds_no_1 = length (makePairingsForAllRounds players) @?= 9
    where players = map (\i -> Player i ("P" ++ show i) 1800 Available) [1..10]
                    
test_rr_rounds_no_2 = length (makePairingsForAllRounds players) @?= 3
    where players = map (\i -> Player i ("P" ++ show i) 1800 Available) [1..3]

