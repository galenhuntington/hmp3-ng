-- Copyright (c) Don Stewart 2004-2008.
-- Copyright (c) Tuomo Valkonen 2004.
-- Copyright (c) 2019-2026 Galen Huntington
-- SPDX-License-Identifier: GPL-2.0-or-later

module Main where

import Base

import Core     (start, shutdown, Options(..))
import qualified Config
import Keymap   (keyLoop)
import Playlist (buildPlaylist, isEmpty)

import System.IO            (hPrint, stderr)
import System.Posix.Signals (installHandler, sigTERM, sigPIPE, sigINT, sigHUP
                            ,sigALRM, sigABRT, Handler(Ignore, Default, Catch))

import qualified Data.ByteString.UTF8 as UTF8

import Options.Applicative

-- ---------------------------------------------------------------------
-- | Set up the signal handlers

initSignals :: IO ()
initSignals = do
    -- ignore
    for_ [sigPIPE, sigALRM] \sig ->
        installHandler sig Ignore Nothing
    -- and exit if we get the following
    for_ [sigINT, sigHUP, sigABRT, sigTERM] \sig ->
        installHandler sig (Catch exitHandler) Nothing

exitHandler :: IO ()
exitHandler = do
    releaseSignals  -- in case shutdown itself gets stuck
    catch @SomeException (shutdown Nothing) (hPrint stderr)
    exitWith $ ExitFailure 1

releaseSignals :: IO ()
releaseSignals =
    for_ [sigINT, sigPIPE, sigHUP, sigABRT, sigTERM] \sig ->
        installHandler sig Default Nothing

------------------------------------------------------------------------
-- | Command-line parsing.

-- | The options together with the file/directory arguments.
invocation :: Parser (Options, [ByteString])
invocation = (,) <$> opts <*> files
  where
    opts = Options
        <$> switch
            (long "paused" <> short 'P' <> help "Start in a paused state")
        <*> optional (strOption -- temporarily internal since feature needs work
            (long "config" <> short 'c' <> metavar "FILE" <> internal
                <> help "Read this config file instead of the XDG default"))
    files = some $ argument (UTF8.fromString <$> str) (metavar "FILE|DIR...")

parserInfo :: ParserInfo (Options, [ByteString])
parserInfo = info (invocation <**> versionOpt <**> helper) $
       fullDesc
    <> header Config.versinfo
    <> progDesc "Play mp3 files in a curses interface."
  where
    versionOpt = infoOption Config.versinfo
        (hidden <> long "version" <> short 'V' <> help "Show version information")

------------------------------------------------------------------------

main :: IO ()
main = do
    (opts, args) <- customExecParser (prefs showHelpOnEmpty) parserInfo
    list <- buildPlaylist args
    when (isEmpty list) $
        errorWithoutStackTrace "Error: No music files found."
    initSignals
    err <- either id absurd <$> try @SomeException do
        start opts list
        keyLoop
    shutdown $ Just $ "Error: " ++ show err

