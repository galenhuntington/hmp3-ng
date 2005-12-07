-- 
-- Copyright (c) Don Stewart 2004-5.
-- Copyright (c) Tuomo Valkonen 2004.
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

import Core     (start, readSt, shutdown)
import Tree     (SerialT(..))
import Utils    ((<+>))
import Config   (darcsinfo, help, versinfo)
import FastIO   (packedGetArgs)
import Keymap   ({-# bogus import to work around 6.4 rec modules bug #-})

import qualified Data.FastPackedString as P (pack,FastString)

import Control.Exception    (catch)

import System.IO            (hPutStrLn, stderr)
import System.Exit          (ExitCode(..), exitWith)
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
    flip mapM_ [sigPIPE, sigALRM] 
               (\sig -> installHandler sig Ignore Nothing)

    -- and exit if we get the following:
    flip mapM_ [sigINT, sigHUP, sigABRT, sigTERM] $ \sig -> do
            installHandler sig (Catch (do
                Control.Exception.catch (shutdown Nothing) (\f -> hPutStrLn stderr (show f))
                exitWith (ExitFailure 1) )) Nothing

releaseSignals :: IO ()
releaseSignals =
    flip mapM_ [sigINT, sigPIPE, sigHUP, sigABRT, sigTERM] 
               (\sig -> installHandler sig Default Nothing)

------------------------------------------------------------------------
-- | Argument parsing.

-- usage string.
usage :: [String]
usage = ["Usage: hmp3 [-Vh] [FILE|DIR ...]"
        ,"-V  --version  Show version information"
        ,"-h  --help     Show this help"]

-- | Parse the args
do_args :: [P.FastString] -> IO (Either SerialT [P.FastString])
do_args []  = do    -- attempt to read db
    x <- readSt 
    case x of
        Nothing -> do mapM_ putStrLn usage; exitWith ExitSuccess
        Just st -> return $ Left st

do_args [s] | s == P.pack "-V"  || s == P.pack "--version"
            = do putStrLn (versinfo <+> help); putStrLn darcsinfo; exitWith ExitSuccess
            | s == P.pack "-h"  || s == P.pack "--help"
            = do putStrLn (versinfo <+> help); mapM_ putStrLn usage; exitWith ExitSuccess

do_args xs = return $ Right xs

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
    args  <- packedGetArgs
    files <- do_args args
    initSignals
    start files -- never returns

