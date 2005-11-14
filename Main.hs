-- gla-exts required for unsafeCoerce#
-- 
-- Copyright (c) Tuomo Valkonen 2004.
-- Copyright (c) Don Stewarti 2004-5.
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

import Core
import Config
import Keymap ({-# bogus import to work around 6.4 rec modules bug #-})

import Control.Monad            ( when )
import Control.Exception        ( catch )

import System.Posix.Signals
import System.IO
import System.Exit
import System.Environment       ( getArgs )
import System.Console.GetOpt

import GHC.Exception            ( Exception(ExitException) )

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
                Control.Exception.catch shutdown (\f -> hPutStrLn stderr (show f))
                exitWith (ExitFailure 1) )) Nothing

releaseSignals :: IO ()
releaseSignals =
    flip mapM_ [sigINT, sigPIPE, sigHUP, sigABRT, sigTERM] 
               (\sig -> installHandler sig Default Nothing)

-- ---------------------------------------------------------------------
-- | Argument parsing. Pretty standard, except for the trick with -B.
-- The -B flag is needed, and used by Boot.hs to find the runtime
-- libraries. We still parse it here, but ignore it.

data Opts = Help | Version 

options :: [OptDescr Opts]
options = [
    Option ['V']  ["version"] (NoArg Version) "Show version information",
    Option ['h']  ["help"]    (NoArg Help)    "Show this help" ]

--
-- usage string.
--
usage :: IO ()
usage = putStr $ usageInfo "Usage: hmp3 [option...] [file]" options

--
-- deal with real options
--
do_opts :: [Opts] -> IO ()
do_opts (o:_) = case o of
    Help     -> usage    >> exitWith ExitSuccess
    Version  -> putStrLn versinfo >> putStrLn darcsinfo >> exitWith ExitSuccess
do_opts [] = return ()

--
-- everything that is left over
--
do_args :: [String] -> IO [FilePath]
do_args []   = usage    >> exitWith ExitSuccess
do_args args = case (getOpt Permute options args) of
        (o, n, []) -> do
            do_opts o
            return n
        (_, _, errs) -> do mapM_ (hPutStrLn stderr) errs
                           usage
                           exitWith (ExitFailure 1)

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
    Control.Exception.catch
        (      do args  <- getArgs
                  files <- do_args args
                  initSignals
                  start files)

    -- catch any exception thrown by the main loop, clean up and quit
        (\e -> do releaseSignals
                  Control.Exception.catch shutdown (\f -> hPutStrLn stderr (show f))
                  when (not $ isExitCall e) $ hPutStrLn stderr (show e)
                  return ())

    return ()

    where
      isExitCall (ExitException _) = True
      isExitCall _ = False

