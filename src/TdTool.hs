module Main where

import System.Time

data Player = Player { pName :: String, pRating :: Int, pPlace :: String, pDescr :: String }
data Tourney = Tourney { tName :: String, tPlayers :: [Player], tPlace :: String, tDescr :: String }
             
data TournamentType = Swiss | RoundRobin
                    
 -- round number, list of pairs, list of players getting bye in this round                    
data RoundPairings = RoundPairings Int [(Player, Player)] (Maybe [Player])
type Pairings = [RoundPairings]
    
data Game = Game Player Player GameResult ClockTime
data GameResult = NotStarted | Win | Loss | Draw | Adjourned | Cancelled | ForfeitWin | ForfeitLoss

main = putStrLn "Welcome, tournament director."           

