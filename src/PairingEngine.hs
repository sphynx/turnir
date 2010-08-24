-- Td-tool -- a tool for tournament management.
--
-- Author : Ivan N. Veselov
-- Created: 24-Aug-2010, Independence day in Ukraine! :)
--
-- Copyright (C) 2010 Ivan N. Veselov
--
-- License: BSD3
--
-- | Module containing API for pairing engines, this should provide
-- the interface both for engine writes and engine users.
--
module PairingEngine where

-- | Different types of tournament play systems, includes Swiss and RoundRobin
data PlaySystem = DubovSwiss | DutchSwiss | LimSwiss | RoundRobin | DoubleRoundRobin

-- | Tie breaks are the means of resolving ties when two or more players have the
--   same number of points
data TieBreak = Buchholz | Berger

-- | Scores represent the quantity of points awarded for the win, draw and loss
data ScoreType = Classic
                 -- ^ 1 for win, 1/2 for draw, 0 for loss
               | Score Int Int Int
                 -- ^ any other possible scores in the following order (win, draw, loss)

-- | All the data about player needed for engines to work properly, includes ID, rating and Status
data EnginePlayer = P ID Rating Status
type ID = Int
type Rating = Int

-- | Status of a player in the tourney
data Status = Available -- ^ ready to play
            | NotAvailable -- ^ cancelled his
            | Bye -- ^ requested half-point bye or doesn't have a pair (odd number of players case)

-- | Set of various tournament params used by the engine
data TournamentParams = TParams
    { playSystem :: PlaySystem -- ^ play system used
    , tieBreaks :: [TieBreak] -- ^ list of tie breaks in the descending priority
    , rounds :: Int -- ^ number of rounds in Swiss, irrelevany in RoundRobin
    , scoreType :: ScoreType -- ^ type of scoring used
    }
