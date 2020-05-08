-- 
-- Copyright (c) Don Stewart 2004-2008.
-- Copyright (c) Tuomo Valkonen 2004.
-- Copyright (c) 2019, 2020 Galen Huntington
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

import Core     (start, readSt, shutdown)
import Tree     (SerialT(..))
import Utils    ((<+>))
import Config   (darcsinfo, help, versinfo)

import qualified Data.ByteString.Char8 as P (pack,ByteString)

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

releaseSignals :: IO ()
releaseSignals =
    for_ [sigINT, sigPIPE, sigHUP, sigABRT, sigTERM]
        \sig -> installHandler sig Default Nothing

------------------------------------------------------------------------
-- | Argument parsing.

-- usage string.
usage :: [String]
usage = ["Usage: hmp3 [-Vh] [FILE|DIR ...]"
        ,"-V  --version  Show version information"
        ,"-h  --help     Show this help"]

-- | Parse the args
do_args :: [P.ByteString] -> IO (Either SerialT [P.ByteString])
do_args []  = do    -- attempt to read db
    x <- readSt
    case x of
        Nothing -> do traverse_ putStrLn usage; exitSuccess
        Just st -> pure $ Left st

do_args [s] | s == "-V" || s == "--version"
            = do putStrLn (versinfo <+> help); putStrLn darcsinfo; exitSuccess
            | s == "-h" || s == "--help"
            = do putStrLn (versinfo <+> help); traverse_ putStrLn usage; exitSuccess

do_args xs = pure $ Right xs

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
    args  <- map P.pack <$> getArgs
    files <- do_args args
    initSignals
    start files -- never returns

