-- Turnir -- a tool for tournament management.
--
-- Author : Ivan N. Veselov
-- Created: 13-Sep-2010
--
-- Copyright (C) 2010 Ivan N. Veselov
--
-- License: BSD3
--
-- | Contains parsers for reading players data from file, etc.
--
module Parser (
  parsePlayers -- ^ parse players list from given file
  ) where

import Types
import Text.ParserCombinators.Parsec

parsePlayers :: String -> IO (Either ParseError [Player])
parsePlayers = parseFromFile players

players :: Parser [Player]
players = sepEndBy player space >>= return

player :: Parser Player
player = do
    id' <- number <?> "player ID"
    char '.'
    many space
    name <- many1 (letter <|> char ' ' <|> char '.') <?> "player name"
    char ','
    many space
    r <- number <?> "rating"
    return $ Player id' name r Available

number :: Parser Int
number = do
    digits <- many1 digit
    return (read digits)

