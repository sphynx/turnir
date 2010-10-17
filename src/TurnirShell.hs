-- Turnir -- a tool for tournament management.
--
-- Author : Ivan N. Veselov
-- Created: 14-Sep-2010
--
-- Copyright (C) 2010 Ivan N. Veselov
--
-- License: BSD3
--
-- | Shell module, provides interaction with the user.

module TurnirShell(
  run -- ^ run the shell
) where

import Types
import Parser
import Pretty
import ShellUtils
import qualified RoundRobin as RR

import System.Console.Shell
import System.Console.Shell.Backend.Haskeline
import System.Console.Shell.ShellMonad

import Data.IORef

import Control.Monad.Trans(liftIO)

-- main shell state datatype
data ShellState = ShellState {
      stPlayers :: [Player]
    , stMaxPlayerId :: Int
    , stTable :: Table
    }

-- main shell description
mkShellDescr :: ShellDescription ShellState
mkShellDescr = (mkShellDescription cmds react) {
      commandStyle = OnlyCommands
    , greetingText = Just "Welcome to Turnir v0.1, mister tournament director!\n"
    }

-- main shell commands
cmds = [ exitCommand "quit"
       , helpCommand "help"
       , helpCommand "?"
       , cmd "players" playersCmd "Launch players subshell"
       , cmd "rounds" showRoundsCmd "Shows all rounds pairings"
       , cmd "table" showTableCmd "Shows tournament scoretable"
       , cmd "rr" roundRobinCmd "Makes Round-Robin pairings"
       , cmd "result" setResultCmd "Set result of the game. Usage: set <gameId> <result> (0, 1/2, 1, etc.)"
       ]

playersCmd :: Sh ShellState ()
playersCmd = runSubshell playersSubshell

showRoundsCmd :: Sh ShellState ()
showRoundsCmd = do
  st <- getShellSt
  shellPutStrLn . show $ ppRounds (stPlayers st) (stTable st)

showTableCmd :: Sh ShellState ()
showTableCmd = do
  st <- getShellSt
  shellPutStrLn . show $ ppTable (stPlayers st) (stTable st)

roundRobinCmd :: Sh ShellState ()
roundRobinCmd = do
    st <- getShellSt
    let pairings = RR.makePairingsForAllRounds . stPlayers $ st
    shellPutStrLn . show $ ppRounds (stPlayers st) pairings
    modifyShellSt (\st -> st { stTable = pairings })

setResultCmd :: Int -> String -> Sh ShellState ()
setResultCmd gid res = do
    st <- getShellSt
    let table = setGameResult gid (parseGameResult res) (stTable st)
    modifyShellSt (\st -> st { stTable = table })

-- players shell state
data PlayersShellState = PlayersShellState {
      playersList :: [Player]
    , playersMaxId :: Int
    }

-- players shell description
mkPlayersDescr :: ShellDescription PlayersShellState
mkPlayersDescr = (mkShellDescription playersCmds react) {
      commandStyle = OnlyCommands
    , prompt = \_ -> return "players> "
}

-- players shell commands
playersCmds :: [ShellCommand PlayersShellState]
playersCmds = [
      cmd "add" addPlayer "add player"
    , cmd "show" printPlayers "print registered players"
    , cmd "load" loadPlayers "load players from file"
    , helpCommand "help"
    , helpCommand "?"
    , exitCommand "quit"
    , exitCommand "up"
    ]

addPlayer :: String -> Sh PlayersShellState ()
addPlayer name = do
    modifyShellSt
      (\(PlayersShellState ps maxId) ->
        PlayersShellState (Player (maxId + 1) name 1800 Available : ps) (maxId + 1))
    shellPutStrLn (name ++ " added")

printPlayers :: Sh PlayersShellState ()
printPlayers = getShellSt >>= mapM_ (shellPutStrLn . show) . playersList

loadPlayers :: File -> Sh PlayersShellState ()
loadPlayers (File f) = do
    result <- liftIO $ parsePlayers f
    case result of
        Left err -> shellPutStrLn (show err)
        Right ps -> putShellSt (PlayersShellState ps 0)

passPlayersState :: ShellState -> IO PlayersShellState
passPlayersState st =
   return PlayersShellState
          { playersList = stPlayers st
          , playersMaxId = stMaxPlayerId st
          }

returnPlayersState :: PlayersShellState -> ShellState -> IO ShellState
returnPlayersState pst st =
   return st
          { stPlayers = playersList pst
          , stMaxPlayerId = playersMaxId pst
          }

playersSubshell :: IO (Subshell ShellState PlayersShellState)
playersSubshell = simpleSubshellWithState passPlayersState returnPlayersState mkPlayersDescr

-- interaction
react s = shellPutErrLn ("Unknown command: " ++ s)

-- run main shell
run :: IO ()
run = do
    runShell mkShellDescr haskelineBackend (ShellState [] 0 [])
    return ()
