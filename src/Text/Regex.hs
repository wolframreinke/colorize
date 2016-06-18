module Text.Regex where

import Prelude hiding (read)

import Data.Foldable (foldl')

import Data.Text (Text)
import qualified Data.Text as T

import Text.Regex.TDFA.Text
import Text.Regex.TDFA

replaceAll :: Regex -> (Text -> Text) -> Text -> Text
replaceAll re f t =  start end
  where
    (_, end, start) = foldl' go (0, t, id) $ getAllMatches
                    $ (match re t :: AllMatches [] (MatchOffset, MatchLength))

    go :: (Int, Text, Text -> Text) -> (Int, Int) -> (Int, Text, Text -> Text)
    go (ind, read, write) (off, len) =
      let (skip, begin)        = T.splitAt (off - ind) read
          (matched, remaining) = T.splitAt len begin
       in (off + len, remaining, write . (skip `T.append`) . (f matched  `T.append`))

