-- |
-- Module      : Main
-- Copyright   : (c) 2016 Wolfram Reinke
-- License     : BSD3
--
-- Maintainer  : Wolfram Reinke <wolframreinke@web.de>
-- Stability   : experimental
-- Portability : portable
--
-- This module defines the ANSI formatting codes that are supported by
-- @colorize@.
module Colorize.Color (Color(..), Format(..), colorize) where

import Prelude hiding ((++))
import Data.Text (Text)
import qualified Data.Text as T

(++) :: Text -> Text -> Text
(++) = T.append


-- | The supported ANSI colors.
data Color = Black | Red | Green | Yellow | Blue | Purple | Cyan | White
  deriving (Show, Eq, Ord, Enum)

colorAnsiCode :: Color -> Int
colorAnsiCode Black  = 0
colorAnsiCode Red    = 1
colorAnsiCode Green  = 2
colorAnsiCode Yellow = 3
colorAnsiCode Blue   = 4
colorAnsiCode Purple = 5
colorAnsiCode Cyan   = 6
colorAnsiCode White  = 7

-- | The supported ANSI formatting codes.
data Format = Reset | Bold | Dim | Underlined | Blink | Invert | Hidden
            | FG Color | BG Color
  deriving (Show, Eq)

fmtAnsiCode :: Format -> Int
fmtAnsiCode Reset      = 0
fmtAnsiCode Bold       = 1
fmtAnsiCode Dim        = 2
fmtAnsiCode Underlined = 4
fmtAnsiCode Blink      = 5
fmtAnsiCode Invert     = 7
fmtAnsiCode Hidden     = 8
fmtAnsiCode (FG color) = 30 + colorAnsiCode color
fmtAnsiCode (BG color) = 40 + colorAnsiCode color



newtype ANSISeq = ANSISeq [Format]

esc :: Text -> Text
esc = ("\ESC[" ++)

ansiCode :: ANSISeq -> Text
ansiCode (ANSISeq fmts) = let codes = map (T.pack . show . fmtAnsiCode) fmts
                           in esc $ T.intercalate ";" codes ++ "m"

-- | Applies the given ANSI formatting directives to a piece of @Text@ by
--   surrounding it with the corresponding formatting codes.
colorize :: [Format] -> Text -> Text
colorize fmts str = ansiCode (ANSISeq fmts) ++ str ++ ansiCode (ANSISeq [Reset])
