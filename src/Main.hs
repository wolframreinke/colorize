-- |
-- Module      : Main
-- Copyright   : (c) 2016 Wolfram Reinke
-- License     : BSD3
--
-- Maintainer  : Wolfram Reinke <wolframreinke@web.de>
-- Stability   : experimental
-- Portability : portable
--
-- This is the main module the @colorize@ executable.

{-# LANGUAGE LambdaCase #-}

module Main where

import Prelude hiding (getLine, until, head, error)

import Control.Monad (forever, when, mzero)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Trans.Class (lift)

import System.Directory (getHomeDirectory)
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess, exitFailure)
import System.FilePath.Posix ((</>), (<.>))
import System.IO (hFlush, hIsEOF, IOMode(..), openFile, hClose, hPutStrLn)
import System.IO (stdin, stdout, stderr)
import System.IO.Error (tryIOError)

import Data.Char (isSpace)
import Data.List.NonEmpty (nonEmpty, head)
import Data.Text (Text, unpack)
import qualified Data.Text.IO as T (hGetContents, getLine, putStrLn)

import Text.Regex (replaceAllM)
import Text.Regex.TDFA
import Text.Regex.TDFA.Text

import Colorize.Rule
import Colorize.Parsing (parseRuleFile)


-- | Loads a list of 'Rule's from the given rule file name.  This file name is
--   the name of the rules file without the @".rules"@ extension.  If the rule
--   file doesn't exist, cannot be read, or contains syntax errors, an error
--   message is written to @stderr@ and @Nothing@ is returned.
loadRuleFile :: String -> IO (Maybe [Rule])
loadRuleFile name = do
    homeDir  <- getHomeDirectory
    let ruleDir  = homeDir </> ".colorize"
    let filename = ruleDir </> name <.> "rules"

    tryIOError (openFile filename ReadMode) >>= \case

      Left error   -> do
        hPutStrLn stderr $ show error
        return Nothing

      Right handle -> do
        text <- T.hGetContents handle
        hClose handle

        maybePreprocessed <- runMaybeT $ preprocessRuleFile ruleDir text
        case maybePreprocessed of
          Nothing -> do
            hPutStrLn stderr $ "Could not preprocess file: " ++ filename
            return Nothing
          Just preprocessed -> do

            case parseRuleFile name preprocessed of
              Left error  -> do
                hPutStrLn stderr $ show error
                return Nothing
              Right rules -> return (Just rules)


-- | Preprocesses the given text by replacing all include directives with the
--   contents of the files they point to.  For that purpose the rule directory
--   is required as the first parameter.
preprocessRuleFile :: String -> Text -> MaybeT IO Text
preprocessRuleFile ruleDir = replaceAllM regex include
  where
    Right regex = compile defaultCompOpt defaultExecOpt "include [^ ]+"

    include :: Text -> MaybeT IO Text
    include directive = do
        let rulename = extractRuleName directive
        let filename = ruleDir </> rulename <.> "rules"

        lift (tryIOError $ openFile filename ReadMode) >>= \case
          Right handle -> do
            content <- lift $ T.hGetContents handle
            lift $ hClose handle
            preprocessRuleFile ruleDir content
          Left _       -> mzero

    -- the directive has the form "include <rulename>", and this extracts
    -- the rulename from the directive
    extractRuleName = drop 1 . dropWhile (not . isSpace) . unpack



main :: IO ()
main = do
    maybeArgs <- nonEmpty <$> getArgs
    case maybeArgs of
      Nothing -> do
        progName <- getProgName
        hPutStrLn stderr $ "Usage: " ++ progName ++ " <rule name>"
        exitFailure

      Just args -> do
        let rulename = head args
        loadRuleFile rulename >>= \case
          Nothing    -> exitFailure
          Just rules -> forever $ do
            isEOF <- hIsEOF stdin
            when isEOF exitSuccess

            line <- T.getLine
            T.putStrLn $ applyRules rules line
            hFlush stdout
