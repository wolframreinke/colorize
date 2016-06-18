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

module Main where

import Prelude hiding (getLine, until, head)

import Colorize.Rule
import Colorize.Parsing (parseRuleFile)

import Control.Monad (forever, when)

import System.Environment (getArgs)
import System.Exit (exitSuccess, exitFailure)
import System.IO (hFlush, hIsEOF, IOMode(..), openFile, hClose, hPutStrLn)
import System.IO (stdin, stdout, stderr)

import Data.List.NonEmpty (nonEmpty, head)
import qualified Data.Text.IO as T (hGetContents, getLine, putStrLn)


main :: IO ()
main = do
    maybeArgs <- nonEmpty <$> getArgs
    case maybeArgs of
      Nothing -> do
        hPutStrLn stderr "Filename required"
        exitFailure

      Just args -> do
        let filename = head args
        handle <- openFile filename ReadMode
        text   <- T.hGetContents handle
        hClose handle

        case parseRuleFile filename text of
          Left  err   -> putStrLn $ show err
          Right rules -> forever $ do
            isEOF <- hIsEOF stdin
            when isEOF exitSuccess

            line <- T.getLine
            T.putStrLn $ applyRules rules line
            hFlush stdout
