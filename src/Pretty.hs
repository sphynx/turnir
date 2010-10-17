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
  ppTable,
  ppRounds
) where

import Text.PrettyPrint.HughesPJ

import Types

--
-- Helper functions
--

t :: String -> Doc
t = text

pp :: Show a => a -> Doc
pp = t . show

dash = char '-'
plus = char '+'
pipe = char '|'

vpunctuate :: Doc -> [Doc] -> [Doc]
vpunctuate p []     = []
vpunctuate p (d:ds) = go d ds
    where
       go d [] = [d]
       go d (e:es) = (d $$ p) : go e es

--
-- Pretty-printing
--

-- | Prints one round information
ppRound :: Int -> [Player] -> Table -> Doc
ppRound r ps table = vcat [ t "Round" <+> int r
                          , nest o (ppGames games)
                          , nest o (ppByes byes)
                          , space
                          ]
    where ppGames = vcat . map ppGame
          ppGame (Game gid _ p1 p2 res) =
              hsep [int gid <> colon, pp p1, dash, pp p2, parens . pp $ res]
          ppByes [] = empty
          ppByes bs = hsep . (t "bye:" :) . map pp $ bs
          games = roundGames r table
          byes = roundByes r ps table
          o = 2 -- outline of games

-- | Pretty-prints all the rounds, using players list
ppRounds :: [Player] -> Table -> Doc
ppRounds ps table =
    vcat . map (\r -> ppRound r ps table) $ [1 .. maxRound table]

-- | Pretty-prints all the rounds, using players list
ppTable :: [Player] -> Table -> Doc
ppTable ps t =
  table (header : cells)
  where
    header = "name" : map show [1 .. n]
    cells = map (\i -> playerName (ps !! i) : map (\j -> result i j t) [0 .. n - 1]) [0 .. n - 1]
    n = length ps
    result i j t = pp $ gameByPlayers (ps !! i) (ps !! j) t
    pp Nothing = " "
    pp (Just x) = show . gameResult $ x

--
-- Pretty-printing textual tables
--

-- | helper functions, ecloses list with pluses or pipes
pluses xs = plus <> xs <> plus
pipes xs = pipe <+> xs <+> pipe

-- | return widths of columns, currently width is unbound
widths :: [[String]] -> [Int]
widths = map (+2) . foldl1 (zipWith max) . map (map length)

-- | makes separator doc
s :: [[String]] -> Doc
s = pluses . hcat . punctuate plus . map (t . flip replicate '-') . widths

-- | makes values doc (row with values, separated by "|")
v :: [Int]  -- ^ list which contains width of every column
     -> [String]  -- ^ list with cells
     -> Doc
v ws dt = pipes . hcat . punctuate (t " | ") $ zipWith fill ws dt

-- | `fills` string to make it of the given length (actually adds spaces)
--   currently, it adds spaces to the right, eventually alignment will be used
fill :: Int -> String -> Doc
fill n s
    | length s < n = t s <> hcat (replicate (n - length s - 2) space)
    | otherwise    = t (take n s)

-- | pretty prints table with data (first list is header row, next ones are rows with data)
table dt = sepRow $$ (vcat . vpunctuate sepRow $ map (v ws) dt) $$ sepRow
  where
   sepRow = s dt
   ws = widths dt

-- test data
headers = ["ID", "Name", "Price"]
dt1 = ["1", "iPad", "12.00"]
dt2 = ["2", "Cool laptop", "122.00"]
dt3 = ["3", "Yet another cool laptop", "12004.44"]
t1 = [headers, dt1, dt2, dt3]
