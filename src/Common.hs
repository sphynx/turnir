-- Td-tool -- a tool for tournament management.
--
-- Author : Ivan N. Veselov
-- Created: 24-Aug-2010
--
-- Copyright (C) 2010 Ivan N. Veselov
--
-- License: BSD3
--
-- Common data structures.

module Common where

import Data.List

data Player = Player { pName :: String
                     , pRating :: Int
                     , pPlace :: String
                     , pDescr :: String
                     }

instance Show Player where
    show (Player name _ _ _) = name

data Tournament = Tournament { tName :: String
                             , tPlayers :: [Player]
                             , tPlace :: String
                             , tDescr :: String
                             } deriving Show

data TournamentType = Swiss | RoundRobin

-- round number, list of pairs, list of players getting bye in this round
data RoundPairings = RoundPairings Int [(Player, Player)] [Player]

showPairs = concatMap (\(p1, p2) -> show p1 ++ " - " ++ show p2 ++ "\n")
showByes [] = ""
showByes bs = "bye: " ++ (concatMap (\p -> show p ++ " ") bs)

instance Show RoundPairings where
    show (RoundPairings n ps byes) = "Round " ++ show n ++ "\n\n" ++ showPairs ps ++ showByes byes ++ "\n"

type Pairings = [RoundPairings]

data Game = Game Player Player GameResult

data GameResult = NotStarted | Win | Loss | Draw | Adjourned | Cancelled | ForfeitWin | ForfeitLoss
