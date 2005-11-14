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
import Utils
import Config
import Keymap ({-# bogus import to work around 6.4 rec modules bug #-})

import Data.List ( sort )
import qualified Data.FastPackedString as P

import Control.Monad
import Control.Exception        ( catch )

import System.IO
import System.Exit
import System.Posix.Signals

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

------------------------------------------------------------------------
-- | Argument parsing.

-- usage string.
usage :: [String]
usage = ["Usage: hmp3 [option...] [file|dir]"
        ,"-V  --version  Show version information"
        ,"-h  --help     Show this help"]

-- | Parse the args
do_args :: [P.FastString] -> IO [P.FastString]
do_args []  = do mapM_ putStrLn usage; exitWith ExitSuccess
do_args [s] | s == P.pack "-V" 
            = do putStrLn versinfo; putStrLn darcsinfo; exitWith ExitSuccess
            | s == P.pack "-h" 
            = do mapM_ putStrLn usage; exitWith ExitSuccess
do_args xs = return xs

-- ---------------------------------------------------------------------
-- Expand directory arguments into their contents
--
expand :: [P.FastString] -> IO [P.FastString]
expand []     = return []
expand (f:fs) = do
        ls  <- expand' f
        lss <- expand fs
        return $! ls ++ lss -- hmm
    where
      expand' :: P.FastString -> IO [P.FastString]
      expand' g = do
            b  <- doesFileExist g'
            if not b
                then do ls  <- liftM (filter notEdge) $! packedGetDirectoryContents g'
                        let ls' = map buildp ls
                        gs  <- filterM doesFileExist ls'
                        ds  <- filterM doesDirectoryExist ls'
                        gs' <- expand (sort ds) -- expand in sorted order
                        return (gs ++ gs')
                else return [g']

            where g' = packedFileNameEndClean g
                  buildp h = g' `P.append` P.packAddress "/"# `P.append` h
                  notEdge p = p /= P.packAddress "."# && p /= P.packAddress ".."#

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
        (      do args  <- packedGetArgs
                  files <- do_args args
                  files'<- expand files
                  case files' of
                    [] -> do mapM_ putStrLn usage; exitWith (ExitFailure 1)
                    fs -> initSignals >> start fs)

    -- catch any exception thrown by the main loop, clean up and quit
        (\e -> do releaseSignals
                  Control.Exception.catch shutdown (\f -> hPutStrLn stderr (show f))
                  when (not $ isExitCall e) $ hPutStrLn stderr (show e)
                  return ())

    return ()

    where
      isExitCall (ExitException _) = True
      isExitCall _ = False

