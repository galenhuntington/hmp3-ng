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
import qualified UI

import Control.Monad            ( when )
import Control.Exception        ( catch, throw )

import System.Console.GetOpt
import System.Environment       ( getArgs )
import System.Exit
import System.Posix.Signals
import System.Posix.Process

import GHC.Exception            ( Exception(ExitException) )

#include "config.h"

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
    -- we have to do our own quitE here.
    flip mapM_ [sigINT, sigHUP, sigABRT, sigTERM] $ \sig -> do
            installHandler sig (CatchOnce $ do
                    Control.Exception.catch shutdown (\_ -> return ())
                    Control.Exception.catch UI.end   (\_ -> return ())
                    (exitImmediately (ExitFailure 1))) Nothing

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
usage, versinfo :: IO ()
usage = putStr $ usageInfo "Usage: hmp3 [option...] [file]" options

package :: String
package = "hmp3"

version :: String
version = "0"

versinfo = do 
        putStrLn $ package++" "++
                   version++"p"++(show (PATCH_COUNT :: Int))
        putStrLn $ "darcs get "++ REPO_PATH

--
-- deal with real options
--
do_opts :: [Opts] -> IO ()
do_opts (o:_) = case o of
    Help     -> usage    >> exitWith ExitSuccess
    Version  -> versinfo >> exitWith ExitSuccess
do_opts [] = return ()

--
-- everything that is left over
--
do_args :: [String] -> IO [FilePath]
do_args args =
    case (getOpt Permute options args) of
        (o, n, []) -> do
            do_opts o
            return n
        (_, _, errs) -> error (concat errs)

-- ---------------------------------------------------------------------
-- | Static main. This is the front end to the statically linked
-- application, and the real front end, in a sense. 'dynamic_main' calls
-- this after setting preferences passed from the boot loader.
--
-- Initialise the ui getting an initial editor state, set signal
-- handlers, then jump to ui event loop with the state.
--
main :: IO ()
main = Control.Exception.catch
        (do args  <- getArgs
            files <- do_args args
            initSignals
            start files)

    -- catch any exception thrown by the main loop, clean up and quit
    -- (catching an ExitException)
        (\e -> do releaseSignals
                  Control.Exception.catch shutdown (\_ -> return ())
                  when (not $ isExitCall e) $ print e
                  throw e)

    where
      isExitCall (ExitException _) = True
      isExitCall _ = False

