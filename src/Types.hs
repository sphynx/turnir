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

-- | Game along with its participants and result
data Game = Game
    { gameId :: GameID -- ^ ID of the game
    , white :: Player -- ^ white player
    , black :: Player -- ^ black player
    , gameResult :: GameResult -- ^ result of the game
    } deriving (Eq, Show)
type GameID = Int
instance Ord Game where
    (Game gid1 _ _ _) <= (Game gid2 _ _ _) = gid1 <= gid2

-- | Game result. Besides usual win/draw/loss, there are a couple of non-standard results involded
data GameResult = NotStarted | Win | Loss | Draw | Adjourned | Cancelled | ForfeitWin | ForfeitLoss
                deriving (Show, Eq)

-- | Represents pairings for one round
data RoundPairings = RoundPairings {
     pRoundNo :: Int -- ^ round number
   , pGames :: [Game] -- ^ list of schedules games
   , pByes :: [Player] -- ^ list of players getting bye in this round
   } deriving Show

-- | Pretty printing for pairings (well, it's not actually very pretty, should eventually
-- switch to some specific PP library).
ppPairings (RoundPairings n games byes) =
    "Round " ++ show n ++ "\n\n" ++ showPairs games ++ showByes byes ++ "\n"
    where showPairs = concatMap (\(Game _ p1 p2 _) -> show p1 ++ " - " ++ show p2 ++ "\n")
          showByes [] = ""
          showByes bs = "bye: " ++ concatMap (\p -> show p ++ " ") bs

type Pairings = [RoundPairings]

-- | Pairing engine itself
data PairingEngine st = PairingEngine
    -- ^ engine state passed as a parameter, it will be initialized during @initEngine@
   { initEngine    :: [Player] -> TournamentParams -> st
                   -- ^ Initializes the engine with players list and tournament parameters

   , makePairings  :: st -> Maybe Int -> Pairings
                   -- ^ Make pairings for given round (for example @Just 2@) or
                   -- for all the rounds (@None@)

   , setGameResult :: st -> GameID -> GameResult
                   -- ^ Sets the game result for the sheduled game
   }
