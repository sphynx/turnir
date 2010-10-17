-- Turnir -- a tool for tournament management.
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
data ScoreType = ScoreType
                 Float -- ^ win
                 Float -- ^ draw
                 Float -- ^ loss

-- | All the data about player needed for engines to work properly, includes ID, names, rating and status
data Player = Player
              { playerId :: Int -- ^ Player ID to use for further reference
              , playerName :: String -- ^ Player name
              , playerRating :: Int -- ^ Rating
              , playerStatus :: Status -- ^ Status
              }

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

playerGames :: Player -> Table -> [Game]
playerGames p = filter (\g -> p == white g || p == black g)

playerScore :: Player -> Game -> Float
playerScore p g
  | white g == p = result2points . gameResult $ g
  | black g == p = result2points . resultNegate . gameResult $ g
  | otherwise = 0

playerTotal :: Player -> Table -> Float
playerTotal p = sum . map (playerScore p) . playerGames p

gameById :: GameID -> Table -> Maybe Game
gameById gid t = case filter (\g -> gameId g == gid) t of
  []       -> Nothing
  game : _ -> Just game

gameByPlayers :: Player -> Player -> Table -> Maybe Game
gameByPlayers p1 p2 t =
  case filter (\g ->
                (white g == p1 && black g == p2) || (white g == p2 && black g == p1)) t of
    []       -> Nothing
    game : _ -> Just game

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
data GameResult = NotStarted | Win | Loss | Draw | Adjourned | Cancelled | ForfeitWin | ForfeitLoss | ForfeitDraw
                deriving (Show, Eq)

data GameScore = NoPoints | PartOfPoints | AllPoints

result2score :: GameResult -> GameScore
result2score NotStarted = NoPoints
result2score Win = AllPoints
result2score Loss = NoPoints
result2score Draw = PartOfPoints
result2score Adjourned = NoPoints
result2score Cancelled = NoPoints
result2score ForfeitWin = AllPoints
result2score ForfeitLoss = NoPoints
result2score ForfeitDraw = PartOfPoints

resultNegate :: GameResult -> GameResult
resultNegate Win = Loss
resultNegate Loss = Win
resultNegate ForfeitWin = ForfeitLoss
resultNegate ForfeitLoss = ForfeitWin
resultNegate x = x

score2points :: ScoreType -> GameScore -> Float
score2points (ScoreType w _ _) AllPoints = w
score2points (ScoreType _ d _) PartOfPoints = d
score2points (ScoreType _ _ l) NoPoints = l

classicScore :: ScoreType
classicScore = ScoreType 1 0.5 0

result2points :: GameResult -> Float
result2points = score2points classicScore . result2score

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
                                 else g : updateGames gid res gs

