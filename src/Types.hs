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
module Types where

import Data.List

-- | Different types of tournament play systems, includes various Swiss systems and RoundRobin
data PlaySystem = DubovSwiss | DutchSwiss | LimSwiss | RoundRobin | DoubleRoundRobin

-- | Tie breaks are the means of resolving ties when two or more players have the
--   same number of points
data TieBreak = Buchholz | Berger

-- | Scores represent the quantity of points awarded for the win, draw and loss
data ScoreType = Classic
                 -- ^ 1 for win, 1/2 for draw, 0 for loss
               | Score Int Int Int
                 -- ^ any other possible scores in the following order (win, draw, loss)

-- | All the data about player needed for engines to work properly, includes ID, names, rating and status
data Player = Player
              Int -- ^ Player ID to use for further reference
              String -- ^ Player name
              Int -- ^ Rating
              Status -- ^ Status

instance Show Player where
    show (Player _ name _ _) = name
instance Eq Player where
    Player id1 _ _ _ == Player id2 _ _ _ = id1 == id2
instance Ord Player where
    Player id1 _ _ _ <= Player id2 _ _ _ = id1 <= id2

-- | Status of a player in the tourney
data Status = Available -- ^ ready to play
            | NotAvailable -- ^ cancelled his tourney participation
            | Bye -- ^ requested half-point bye or doesn't have a pair (odd number of players case)
            deriving Show

-- | Set of various tournament params used by the engine
data TournamentParams = TParams
    { playSystem :: PlaySystem -- ^ play system used
    , tieBreaks :: [TieBreak] -- ^ list of tie breaks in the descending priority
    , rounds :: Int -- ^ number of rounds in Swiss, irrelevany in RoundRobin
    , scoreType :: ScoreType -- ^ type of scoring used
    }

-- | Tournament itself.
data Tournament = Tournament
    { tPlayers :: [Player] -- ^ list of players
    , tTable :: Table -- ^ score table
    , params :: TournamentParams -- ^ params of the tourney
    }

-- | Tournament table is represented as list of the games
type Table = [Game]

roundGames :: Int -> Table -> [Game]
roundGames r = filter (\g -> r == roundId g)

roundByes :: Int -> [Player] -> Table -> [Player]
roundByes r ps = (ps \\) . concatMap (\g -> [white g, black g]) . roundGames r

maxRound :: Table -> Int
maxRound [] = 0
maxRound t = maximum . map roundId $ t

-- | Game along with its participants and result
data Game = Game
    { gameId :: GameID -- ^ ID of the game
    , roundId :: Int -- ^ number of the round
    , white :: Player -- ^ white player
    , black :: Player -- ^ black player
    , gameResult :: GameResult -- ^ result of the game
    } deriving (Eq, Show)
type GameID = Int
instance Ord Game where
    g1 <= g2 = gameId g1 <= gameId g2

-- | Game result. Besides usual win/draw/loss, there are a couple of non-standard results involded
data GameResult = NotStarted | Win | Loss | Draw | Adjourned | Cancelled | ForfeitWin | ForfeitLoss
                deriving (Show, Eq)

parseGameResult :: String -> GameResult
parseGameResult "1" = Win
parseGameResult "0" = Loss
parseGameResult "1/2" = Draw
parseGameResult "0.5" = Draw
parseGameResult _ = NotStarted

setGameResult :: GameID -> GameResult -> Table -> Table
setGameResult gid result t = updateGames gid result t
  where
    updateGames _ _ [] = []
    updateGames gid res (g:gs) = if (gameId g == gid)
                                 then g {gameResult = result} : gs
                                 else updateGames gid res gs

-- | Pretty printing for round games (well, it's not actually very pretty, should eventually
-- switch to some specific PP library).
ppRound r ps table =
    "Round " ++ show r ++ "\n\n" ++ showPairs games ++ showByes byes ++ "\n"
    where showPairs = concatMap (\(Game gid _ p1 p2 _) -> show gid ++ ": " ++ show p1 ++ " - " ++ show p2 ++ "\n")
          showByes [] = ""
          showByes bs = "bye: " ++ concatMap (\p -> show p ++ " ") bs
          games = roundGames r table
          byes = roundByes r ps table

ppTable ps table =
    map (\r -> ppRound r ps table) [1 .. (maxRound table)]
