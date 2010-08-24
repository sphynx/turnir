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
data EnginePlayer = EnginePlayer ID Rating Status
type ID = Int
type Rating = Int
instance Show EnginePlayer where
    show (EnginePlayer pid _ _) = show pid

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

-- | Game along with its participants and result
data Game = Game
    { gameId :: GameID -- ^ ID of the game
    , white :: EnginePlayer -- ^ white player
    , black :: EnginePlayer -- ^ black player
    , gameResult :: GameResult -- ^ result of the game
    }
type GameID = Int

-- | Game result. Besides usual win/draw/loss, there are a couple of non-standard results involded
data GameResult = NotStarted | Win | Loss | Draw | Adjourned | Cancelled | ForfeitWin | ForfeitLoss

-- | Represents pairings for one round
data RoundPairings = RoundPairings
   Int -- ^ round number
   [Game] -- ^ list of schedules games
   [EnginePlayer] -- ^ list of players getting bye in this round

-- | Pretty printing for pairings (well, it's not actually very pretty, should eventually
-- switch to some specific PP library).
ppPairings (RoundPairings n games byes) =
    "Round " ++ show n ++ "\n\n" ++ showPairs games ++ showByes byes ++ "\n"
    where showPairs = concatMap (\(Game _ p1 p2 _) -> show p1 ++ " - " ++ show p2 ++ "\n")
          showByes [] = ""
          showByes bs = "bye: " ++ (concatMap (\p -> show p ++ " ") bs)

type Pairings = [RoundPairings]

-- | Pairing engine itself
data PairingEngine = PairingEngine
   { initEngine    :: [EnginePlayer] -> TournamentParams -> PairingEngine
                   -- ^ Initializes the engine with players lost and tournament parameters

   , makePairings  :: Maybe Int -> Pairings
                   -- ^ Make pairings for given round (for example @Just 2@) or
                   -- for all the rounds (@None@)

   , setGameResult :: GameID -> GameResult -> PairingEngine
                   -- ^ Sets the game result for the sheduled game
   }
