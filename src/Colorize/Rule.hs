module Colorize.Rule where

import Colorize.Color

import Data.Text (Text)
import Text.Regex.TDFA
import Text.Regex


data Rule = Colorize Regex [Format]
          | With     Regex [Rule]

instance Show Rule where
    show (Colorize _ fmts)  = "Colorize /.../ " ++ show fmts
    show (With     _ rules) = "With /.../ {" ++ show rules ++ "}"


applyRules :: [Rule] -> Text -> Text
applyRules   []   t = t
applyRules (r:rs) t = applyRules rs $ applyRule r t


applyRule :: Rule -> Text -> Text
applyRule (Colorize regex fmts)  t = replaceAll regex (colorize fmts) t
applyRule (With     regex rules) t = replaceAll regex (applyRules rules) t
