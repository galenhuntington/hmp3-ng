{-# OPTIONS -cpp #-}
-- 
-- Copyright (C) 2003 Don Stewart - http://www.cse.unsw.edu.au/~dons
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
--
-- The POpen module provides functionality to ``open'' a process by
-- creating two pipes, forking, and executing a file. *Two pipes* are
-- created for *bidirectional* communication with the subprocess, unlike
-- the popen in the library. This is really popen2.
--
-- The return value of the popen command is a tuple of Handle for
-- reading and writing to the subprocess
--
-- The inspiration for this code is J Petersen's hslibs/posix/POpen.hs
--

module POpen (popen) where

import System.Posix.Types   ( ProcessID, Fd )
import System.Posix.Process ( forkProcess, executeFile )
import System.Posix.IO      ( createPipe, stdInput, stdError,
                              stdOutput, closeFd, dupTo )

--
-- provide similar functionality to popen(3), 
-- along with bidirectional ipc via pipes
-- return's the pid of the child process
--
-- there are two different forkProcess functions. the pre-620 was a
-- unix-fork style function, and the modern function has semantics more
-- like the Awkward-Squad paper. We provide implementations of popen
-- using both versions, depending on which GHC the user wants to try.
--
-- And now a third, we return stderr.
-- 
popen :: FilePath -> [String] -> IO (Fd, Fd, Fd, ProcessID)
popen cmd args = do
        (pr, pw)   <- createPipe
        (cr, cw)   <- createPipe    
        (cre, cwe) <- createPipe    

        -- parent --
        let parent = do closeFd cw
                        closeFd cwe
                        closeFd pr
        -- child --
        let child  = do closeFd pw
                        closeFd cr 
                        closeFd cre
                        exec cmd args (pr,cw,cwe)
                        error "exec cmd failed!" -- typing only

#if __GLASGOW_HASKELL__ >= 601
        pid <- forkProcess child -- fork child
        parent                   -- and run parent code
#else
        p   <- forkProcess
        pid <- case p of
                Just pid -> parent >> return pid
                Nothing  -> child
#endif

   --   hcr <- fdToHandle cr
   --   hpw <- fdToHandle pw

        return (cr,pw,cre,pid)

--
-- execve cmd in the child process, dup'ing the file descriptors passed
-- as arguments to become the child's stdin and stdout.
--
exec :: FilePath -> [String] -> (Fd,Fd,Fd) -> IO ()
exec cmd args (pr,cw,ce) = do
        dupTo pr stdInput
        dupTo cw stdOutput      -- dup stderr too!
        dupTo ce stdError       -- dup stderr too!
        executeFile cmd False args Nothing

