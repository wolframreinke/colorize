{-# LANGUAGE ScopedTypeVariables #-}

module Text.Regex where

import Prelude hiding (read)

import Data.Foldable (foldl')

import Data.Text (Text)
import qualified Data.Text as T

import Text.Regex.TDFA.Text
import Text.Regex.TDFA

-- | @replaceAll regex f text@ replaces every piece of text @t@ that matches
--   @regex@ with @f t@.
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


-- | @replaceAllM@ works exactly like @replaceAll@, but it allows monadic
--   actions as the replacement function.  This makes it possible, e.g. to
--   replace filenames with their file contents.
replaceAllM :: forall m. Monad m => Regex -> (Text -> m Text) -> Text -> m Text
replaceAllM re f t =  start end
  where
    (_, end, start) = foldl' go (0, t, return) $ getAllMatches
                    $ (match re t :: AllMatches [] (MatchOffset, MatchLength))

    go :: (Int, Text, Text -> m Text) -> (Int, Int)
       -> (Int, Text, Text -> m Text)
    go (ind, read, write) (off, len) =
      let (skip, begin)        = T.splitAt (off - ind) read
          (matched, remaining) = T.splitAt len begin
       in (off + len, remaining, \x -> do
            replacement <- f matched
            let result = T.append replacement x
            write $ T.append skip result)
