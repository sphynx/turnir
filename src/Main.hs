-- Td-tool -- a tool for tournament management.
--
-- Author : Ivan N. Veselov
-- Created: 23-Aug-2010
--
-- Copyright (C) 2010 Ivan N. Veselov
--
-- License: BSD3
--
-- | Main module, the entry point.

module Main where

import PairingEngine
import qualified RoundRobin as RR

import System.Console.Shell
import System.Console.Shell.Backend.Haskeline
import System.Console.Shell.ShellMonad

data ShellState = ShellState [Player] Int

-- shell interaction
react :: String -> Sh ShellState ()
react s = shellPutErrLn ("Unknown command: " ++ s)

-- known commands
cmds = [ exitCommand "quit"
       , helpCommand "help"
       , cmd "add" addPlayerCmd "Adds a new player"
       , cmd "show" showPlayersCmd "Shows currently registered players"
       , cmd "rr" roundRobinCmd "Makes Round-Robin pairings"
       ]

addPlayerCmd :: String -> Sh ShellState ()
addPlayerCmd name = do
    modifyShellSt (\(ShellState ps maxId) -> ShellState (Player (maxId + 1) name 1800 Available : ps) (maxId + 1))
    shellPutStrLn (name ++ " added")

showPlayersCmd :: Sh ShellState ()
showPlayersCmd = do
    (ShellState ps _) <- getShellSt
    mapM_ (shellPutStrLn . show) ps

roundRobinCmd :: Sh ShellState ()
roundRobinCmd = do
    (ShellState ps _) <- getShellSt
    mapM_ (shellPutStrLn . ppPairings) . RR.makePairingsForAllRounds $ ps

shellDescr :: ShellDescription ShellState
shellDescr = (mkShellDescription cmds react) {
      commandStyle = OnlyCommands
    , greetingText = Just "Welcome to td-tool v0.1, mister tournament director!\n"
    }

main :: IO ()
main = do
    runShell shellDescr haskelineBackend (ShellState [] 0)
    return ()
