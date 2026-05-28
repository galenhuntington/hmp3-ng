-- Copyright (c) Don Stewart 2004-2008.
-- Copyright (c) Tuomo Valkonen 2004.
-- Copyright (c) 2019-2026 Galen Huntington
-- SPDX-License-Identifier: GPL-2.0-or-later

module Main where

import Base

import Core     (start, shutdown, Options(..))
import qualified Config
import Tree     (buildTree, isEmpty)

import System.IO            (hPrint, stderr)
import System.Posix.Signals (installHandler, sigTERM, sigPIPE, sigINT, sigHUP
                            ,sigALRM, sigABRT, Handler(Ignore, Default, Catch))

import qualified Data.ByteString.UTF8 as UTF8

import Options.Applicative

-- ---------------------------------------------------------------------
-- | Set up the signal handlers

--
-- Notes on setStoppedChildFlag:
--      If this bit is set when installing a catching function for the SIGCHLD
--      signal, the SIGCHLD signal will be generated only when a child process
--      exits, not when a child process stops.
--
-- setStoppedChildFlag True
--
initSignals :: IO ()
initSignals = do

    -- ignore
    for_ [sigPIPE, sigALRM] \sig ->
        installHandler sig Ignore Nothing

    -- and exit if we get the following
    for_ [sigINT, sigHUP, sigABRT, sigTERM] \sig ->
        installHandler sig (Catch (do
            catch (shutdown Nothing) (\ (f :: SomeException) -> hPrint stderr f)
            exitWith (ExitFailure 1) )) Nothing

-- XXX this function is not used
releaseSignals :: IO ()
releaseSignals =
    for_ [sigINT, sigPIPE, sigHUP, sigABRT, sigTERM]
        \sig -> installHandler sig Default Nothing

------------------------------------------------------------------------
-- | Command-line parsing.

-- | The options together with the file/directory arguments.
invocation :: Parser (Options, [ByteString])
invocation = (,) <$> opts <*> files
  where
    opts = Options
        <$> switch
            (long "paused" <> short 'P'
                <> help "Start in a paused state")
        <*> optional (strOption
            (long "config" <> short 'c' <> metavar "FILE"
                <> help "Read this config file instead of the XDG default"))
    files = some $ argument (UTF8.fromString <$> str) (metavar "FILE|DIR...")

parserInfo :: ParserInfo (Options, [ByteString])
parserInfo = info (invocation <**> versionOpt <**> helper) $
       fullDesc
    <> header Config.versinfo
    <> progDesc "Play the given MP3 files and directories in a curses interface."
  where
    versionOpt = infoOption (unwords [Config.versinfo, Config.help])
        (long "version" <> short 'V' <> help "Show version information")

------------------------------------------------------------------------

main :: IO ()
main = do
    (opts, args) <- execParser parserInfo
    initSignals
    tree <- buildTree args
    if isEmpty tree
        then putStrLn "Error: No music files found." *> exitFailure
        else start opts tree -- never returns
