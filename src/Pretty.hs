-- Turnir -- a tool for tournament management.
--
-- Author : Ivan N. Veselov
-- Created: 20-Sep-2010
--
-- Copyright (C) 2010 Ivan N. Veselov
--
-- License: BSD3
--
-- | Pretty printing of miscelanneous data structures.
-- Uses wonderful HughesPJ pretty-printing combinator library.
--
module Pretty (
  ppTable
) where

import Text.PrettyPrint.HughesPJ

import Types

-- helper functions
t :: String -> Doc
t = text

tt :: Show a => a -> Doc
tt = t . show

dash :: Doc
dash = char '-'

-- pretty-printing
ppRound :: Int -> [Player] -> Table -> Doc
ppRound r ps table = (t "Round" <+> int r) $$ nest o (ppPairs games) $$ nest o (ppByes byes)
    where ppPairs = vcat . map ppGame
          ppGame (Game gid _ p1 p2 res) =
            hsep [int gid <> colon, tt p1, dash, tt p2, parens . tt $ res]
          ppByes [] = empty
          ppByes bs = hsep . (t "bye: " :) . punctuate space . map tt $ bs
          games = roundGames r table
          byes = roundByes r ps table
          o = 2 -- outline of games

ppTable ps table =
    vcat . map (\r -> ppRound r ps table) $ [1 .. maxRound table]

