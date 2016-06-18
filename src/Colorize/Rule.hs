-- |
-- Module      : Main
-- Copyright   : (c) 2016 Wolfram Reinke
-- License     : BSD3
--
-- Maintainer  : Wolfram Reinke <wolframreinke@web.de>
-- Stability   : experimental
-- Portability : portable
--
-- This module defines colorization 'Rule's.  There are two types of rules:
--
--    * 'Colorize'-rules.  These simple rules consist of a 'Regex', and a list
--      of 'Format's.  The 'Format's are used to colorize everything that
--      matches the regular expression.
--
--    * 'With'-rules.  These compound rules consist of a regular expression and
--      a number of sub-rules, each of which is applied to matches of the
--      'Regex'.
module Colorize.Rule where

import Colorize.Color

import Data.Text (Text)
import Text.Regex.TDFA
import Text.Regex


-- | A colorization rule.
data Rule = Colorize Regex [Format] -- ^ A simple colorization rule.  The
                                    --   @Format@s will be applied to everything
                                    --   that matches the @Regex@.
          | With     Regex [Rule]   -- ^ A rule that applies its sub-@Rule@s to
                                    --   everything that matches the @Regex@.

instance Show Rule where
    show (Colorize _ fmts)  = "Colorize /.../ " ++ show fmts
    show (With     _ rules) = "With /.../ {" ++ show rules ++ "}"


-- | Applies the given 'Rule's to a piece of @Text@.  The rules are applied in
--   order, which is important, as the inserted ANSI formatting codes will
--   prevent other regular expressions from matching.  For example, consider
--   the following rules:
--
--   @
--      colorize \/ho\/     red
--      colorize \/hihohi\/ blue
--   @
--
--   The first rule is applied first, surrounding any occurrence of @ho@ with
--   the formatting codes.  Consequently, the second rule can never match
--   anything, all @ho@s have been replaced.
applyRules :: [Rule] -> Text -> Text
applyRules   []   t = t
applyRules (r:rs) t = applyRules rs $ applyRule r t


-- | Applies a 'Rule' to a piece of @Text@.  For with-rules, the same caveats
--   apply as for 'applyRules'.
applyRule :: Rule -> Text -> Text
applyRule (Colorize regex fmts)  t = replaceAll regex (colorize fmts) t
applyRule (With     regex rules) t = replaceAll regex (applyRules rules) t
