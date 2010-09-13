-- Turnir -- a tool for tournament management.
--
-- Author : Ivan N. Veselov
-- Created: 14-Sep-2010
--
-- Copyright (C) 2010 Ivan N. Veselov
--
-- License: BSD3
--
-- | Contains utility functions for using with shell.
--

module ShellUtils(runSubshell, simpleSubshellWithState) where

import Data.IORef

import Control.Monad.Trans(liftIO)

import System.Console.Shell
import System.Console.Shell.ShellMonad

-- | Runs subshell given its description
runSubshell sh = do
    subshell <- liftIO sh
    shellSpecial (ExecSubshell subshell)

-- | Creates a simple subshell from state mapping functions
--   and a shell description.
simpleSubshellWithState
               :: (st -> IO subst)       -- ^ A function to generate the initial subshell
                                         --   state from the outer shell state
               -> (subst -> st -> IO st) -- ^ A function to generate a new outer shell state
                                         --   based on subshell state and initial outer state
               -> ShellDescription subst -- ^ A shell description for the subshell
               -> IO (Subshell st subst)
simpleSubshellWithState toSubSt fromSubSt desc = do
  ref <- newIORef undefined
  let toSubSt' st      = writeIORef ref st >> toSubSt st
  let fromSubSt' subSt = readIORef ref >>= \st -> fromSubSt subSt st
  let mkDesc _         = return desc
  return (toSubSt', fromSubSt', mkDesc)


