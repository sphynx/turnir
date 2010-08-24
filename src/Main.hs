-- Td-tool -- a tool for tournament management.
--
-- Author : Ivan N. Veselov
-- Created: 23-Aug-2010
--
-- Copyright (C) 2010 Ivan N. Veselov
--
-- License: BSD3

module Main where

import Common
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
       , cmd "add" addPlayerSF "Adds a new player"
       , cmd "show" showPlayersSF "Shows currently registered players"
       , cmd "rr" roundRobinSF "Make Round-Robin pairings"
       ]

addPlayerSF :: String -> Sh ShellState ()
addPlayerSF name = do
    modifyShellSt (\(ShellState ps maxId) -> ShellState ((Player (maxId + 1) name 1800 "" "") : ps) (maxId + 1))
    shellPutStrLn (name ++ " added")

showPlayersSF :: Sh ShellState ()
showPlayersSF = do
    (ShellState ps _) <- getShellSt
    mapM_ (shellPutStrLn . show) ps

roundRobinSF :: Sh ShellState ()
roundRobinSF = do
    (ShellState ps _) <- getShellSt
    mapM_ (shellPutStrLn . ppPairings) . RR.makePairingsForAllRounds . map toEnginePlayer $ ps

toEnginePlayer :: Player -> EnginePlayer
toEnginePlayer (Player id _ rating _ _) = EnginePlayer id rating Available

shellDescr :: ShellDescription ShellState
shellDescr = (mkShellDescription cmds react) {
      commandStyle = OnlyCommands
    , greetingText = Just "Welcome to td-tool v0.1, mister tournament director!\n"
    }

main :: IO ()
main = do
    runShell shellDescr haskelineBackend (ShellState [] 0)
    return ()
