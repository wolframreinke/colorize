-- |
-- Module      : Main
-- Copyright   : (c) 2016 Wolfram Reinke
-- License     : BSD3
--
-- Maintainer  : Wolfram Reinke <wolframreinke@web.de>
-- Stability   : experimental
-- Portability : portable
--
-- This module defines the parsing rules for rule files.
module Colorize.Parsing (parseRuleFile) where

import Control.Monad (void)

import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T

import Text.Regex.TDFA
import Text.Regex.TDFA.Text

import Text.Megaparsec

import Colorize.Rule
import Colorize.Color


-- | Parses the contents of a rule file into a list of 'Rule's.
--   The first argument to this function is the name of the rule file, which is
--   used in error messages only.
parseRuleFile :: String -> Text -> Either ParseError [Rule]
parseRuleFile filename = runParser ruleFile filename

ruleFile :: Parsec Text [Rule]
ruleFile = some (space *> rule) <* eof

rule :: Parsec Text Rule
rule = try colorizeRule <|> withRule

colorizeRule :: Parsec Text Rule
colorizeRule = do
    space
    void (string "colorize")
    Colorize <$> (space *> regex)
             <*> (space *> formats)

withRule :: Parsec Text Rule
withRule = do
    space
    void $ string "with"
    With <$> (space *> regex)
         <*> (space *> between (string "{") (string "}")
                               (space *> some rule <* space))

regex :: Parsec Text Regex
regex = do
  regexstr <- T.pack <$> between (string "/") (string "/") (some $ noneOf "/")
  case compile defaultCompOpt defaultExecOpt regexstr of
      (Right reg) -> return reg
      (Left  err) -> fail err

formats :: Parsec Text [Format]
formats = catMaybes <$> optional oneFormat `sepBy1` (some spaceChar)

oneFormat :: Parsec Text Format
oneFormat =  try (Bold       <$ string "bold")
         <|> try (Bold       <$ string "dark")
         <|> try (Dim        <$ string "light")
         <|> try (Dim        <$ string "dim")
         <|> try (Underlined <$ string "underlined")
         <|> try (Blink      <$ string "blink")
         <|> try (Blink      <$ string "annoying")
         <|> try (Invert     <$ string "invert")
         <|> try (Hidden     <$ string "hidden")
         <|> try fgcolor
         <|>     bgcolor


bgcolor :: Parsec Text Format
bgcolor = pure BG <*> (string "bg" *> some spaceChar *> color)

fgcolor :: Parsec Text Format
fgcolor = pure FG <*> (optional (string "fg" *> some spaceChar) *> color)

color :: Parsec Text Color
color =  try ( Black  <$ string "black"  )
     <|> try ( Red    <$ string "red"    )
     <|> try ( Green  <$ string "green"  )
     <|> try ( Yellow <$ string "yellow" )
     <|> try ( Blue   <$ string "blue"   )
     <|> try ( Purple <$ string "purple" )
     <|> try ( Cyan   <$ string "cyan"   )
     <|>     ( White  <$ string "white"  )
