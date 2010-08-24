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
import qualified RoundRobin as RR

import System.Console.Shell
import System.Console.Shell.Backend.Haskeline
import System.Console.Shell.ShellMonad

data ShellState = ShellState [Player]

-- generates dummy players list
players :: Int -> [Player]
players n = map (\x -> Player ("P" ++ show x) 1800 "Kyiv" "") [1 .. n]

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
    modifyShellSt (\(ShellState ps) -> ShellState ((Player name 0 "" "") : ps))
    shellPutStrLn (name ++ " added")

showPlayersSF :: Sh ShellState ()
showPlayersSF = do
    (ShellState ps) <- getShellSt
    mapM_ (shellPutStrLn . show) ps

roundRobinSF :: Sh ShellState ()
roundRobinSF = do
    (ShellState ps) <- getShellSt
    mapM_ (shellPutStrLn . ppPairings) . RR.makePairings $ ps

shellDescr :: ShellDescription ShellState
shellDescr = (mkShellDescription cmds react) {
      commandStyle = OnlyCommands
    , greetingText = Just "Welcome to td-tool v0.1, mister tournament director!\n"
    }

main :: IO ()
main = do
    runShell shellDescr haskelineBackend (ShellState [])
    return ()
