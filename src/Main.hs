-- Td-tool -- a tool for tournament management.
--
-- Author : Ivan N. Veselov
-- Created: 23-Aug-2010
--
-- Copyright (C) 2010 Ivan N. Veselov
--
-- License: BSD

module Main where

import Common
import qualified RoundRobin as RR

import System.Console.Shell
import System.Console.Shell.Backend.Haskeline
import System.Console.Shell.ShellMonad

import Control.Monad.Trans

-- generates dummy players list
players :: Int -> [Player]
players n = map (\x -> Player ("P" ++ show x) 1800 "Kyiv" "") [1 .. n]

-- shell interaction
react :: String -> Sh () ()
react s = liftIO $ putStrLn ("OK, got " ++ s)

main :: IO ()
main = do
    putStrLn "Welcome, tournament director! Let's see pairings for Round-Robin 5:"
    mapM_ (putStrLn . show) . RR.makePairings . players $ 5
    runShell (mkShellDescription [] react) haskelineBackend ()
