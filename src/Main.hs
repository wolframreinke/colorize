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

import Control.Monad (forever, when)

import System.Directory (getHomeDirectory)
import System.Environment (getArgs, getProgName)
import System.Exit (exitSuccess, exitFailure)
import System.FilePath.Posix ((</>), (<.>))
import System.IO (hFlush, hIsEOF, IOMode(..), openFile, hClose, hPutStrLn)
import System.IO (stdin, stdout, stderr)
import System.IO.Error (tryIOError)

import Data.List.NonEmpty (nonEmpty, head)
import qualified Data.Text.IO as T (hGetContents, getLine, putStrLn)

import Colorize.Rule
import Colorize.Parsing (parseRuleFile)


-- | Loads a list of 'Rule's from the given rule file name.  This file name is
--   the name of the rules file without the @".rules"@ extension.  If the rule
--   file doesn't exist, cannot be read, or contains syntax errors, an error
--   message is written to @stderr@ and @Nothing@ is returned.
loadRuleFile :: String -> IO (Maybe [Rule])
loadRuleFile name = do
    homeDir  <- getHomeDirectory
    let filename = homeDir </> ".colorize" </> name <.> "rules"

    tryIOError (openFile filename ReadMode) >>= \case

      Left error   -> do
        hPutStrLn stderr $ show error
        return Nothing

      Right handle -> do
        text <- T.hGetContents handle
        hClose handle
        case parseRuleFile name text of
          Left error  -> do
            hPutStrLn stderr $ show error
            return Nothing
          Right rules -> return (Just rules)


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
