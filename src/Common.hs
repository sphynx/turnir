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

data Player = Player {
      pId :: Int
    , pName :: String
    , pRating :: Int
    , pPlace :: String
    , pDescr :: String
    }

instance Show Player where show = pName

data Tournament = Tournament { tName :: String
                             , tPlayers :: [Player]
                             , tPlace :: String
                             , tDescr :: String
                             } deriving Show


