-- 
-- Copyright (c) Don Stewart 2004-2008.
-- Copyright (c) Tuomo Valkonen 2004.
-- Copyright (c) 2019-2025 Galen Huntington
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License as
-- published by the Free Software Foundation; either version 2 of
-- the License, or (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
-- General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
-- 02111-1307, USA.
-- 

module Main where

import Base

import Core     (start, shutdown)
import Config   (help, versinfo)
import Tree     (Tree, buildTree, isEmpty)

import System.IO            (hPrint, stderr)
import System.Posix.Signals (installHandler, sigTERM, sigPIPE, sigINT, sigHUP
                            ,sigALRM, sigABRT, Handler(Ignore, Default, Catch))

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
-- | Argument parsing.

-- usage string.
usage :: [String]
usage = ["Usage: hmp3 [-VhP] [FILE|DIR ...]"
        ,"-V  --version  Show version information"
        ,"-h  --help     Show this help"
        ,"-P  --paused   Start in a paused state"
        ]

-- | Parse the args
doArgs :: [ByteString] -> IO (Bool, Tree)
doArgs = loopArgs True where

    verLine = putStrLn $ unwords [versinfo, help]
    showUsage = traverse_ putStrLn usage

    loopArgs _ [] = do
        putStrLn "Specify at least one file or directory."
        showUsage
        exitFailure

    loopArgs _ (s:xs)
        | s == "-V" || s == "--version"
        = verLine *> exitSuccess
        | s == "-h" || s == "--help"
        = verLine *> showUsage *> exitSuccess
        | s == "-P" || s == "--paused"
        = loopArgs False xs

    loopArgs playNow xs = do
        tree <- buildTree xs
        if isEmpty tree
            then putStrLn "Error: No music files found." *> exitFailure
            else pure (playNow, tree)

-- ---------------------------------------------------------------------
-- | Static main. This is the front end to the statically linked
-- application, and the real front end, in a sense. 'dynamic_main' calls
-- this after setting preferences passed from the boot loader.
--
-- Initialise the ui getting an initial editor state, set signal
-- handlers, then jump to ui event loop with the state.
--
main :: IO ()
main = do
    (playNow, files) <- doArgs . map fromString =<< getArgs
    initSignals
    start playNow files -- never returns

