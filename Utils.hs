-- 
-- Copyright (c) 2003-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
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

-- miscellaneous utilites, including:
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

module Utils where

import FastIO                   (printfPS)

import Data.Char                (isSpace)
import Data.List                (isPrefixOf)

import qualified Data.FastPackedString as P

import System.Time              (diffClockTimes, TimeDiff(tdSec), ClockTime)
import System.IO                (IO, FilePath)

import System.Posix.Types       (Fd(..),ProcessID)
import System.Process.Internals (ProcessHandle(..))
import System.Posix.Process     ( forkProcess, executeFile )
import System.Posix.IO          ( createPipe, stdInput, stdError,
                                  stdOutput, closeFd, dupTo )

------------------------------------------------------------------------

-- some filename manipulation stuff

--
-- | </>, <.> : join two path components
--
infixr 6 </>
infixr 6 <.>
infixr 6 <>
infixr 6 <+>

(</>), (<.>), (<>), (<+>) :: FilePath -> FilePath -> FilePath
[] </> b = b
a  </> b = a ++ "/" ++ b

[] <.> b = b
a  <.> b = a ++ "." ++ b

[] <> b = b
a  <> b = a ++ b

[] <+> b = b
a  <+> b = a ++ " " ++ b

basename :: FilePath -> FilePath
basename p = reverse $ takeWhile (/= '/') $ reverse p

dirname :: FilePath -> FilePath
dirname p  =
    case reverse $ dropWhile (/= '/') $ reverse p of
        [] -> "."
        p' -> p'

dropSuffix :: FilePath -> FilePath
dropSuffix f = reverse . tail . dropWhile (/= '.') $ reverse f

joinPath :: FilePath -> FilePath -> FilePath
joinPath p q =
    case reverse p of
      '/':_ -> p ++ q
      []    -> q
      _     -> p ++ "/" ++ q

------------------------------------------------------------------------

-- | 'dropSpace' takes as input a String and strips spaces from the
--   prefix as well as the postfix of the String. Example:
--
-- > dropSpace "   abc  " ===> "abc"
dropSpace :: [Char] -> [Char]
dropSpace = let f = reverse . dropWhile isSpace in f . f

-- | Drop suffix and prefix quotes from a `shown' string
--
clean :: P.FastString -> P.FastString
clean = P.tail . P.init

-- | Split a list into pieces that were held together by glue.  Example:
--
-- > split ", " "one, two, three" ===> ["one","two","three"]
split :: Eq a => [a] -- ^ Glue that holds pieces together
      -> [a]         -- ^ List to break into pieces
      -> [[a]]       -- ^ Result: list of pieces
split glue xs = split' xs
    where
    split' [] = []
    split' xs' = piece : split' (dropGlue rest)
        where (piece, rest) = breakOnGlue glue xs'
    dropGlue = drop (length glue)


-- | Break off the first piece of a list held together by glue,
--   leaving the glue attached to the remainder of the list.  Example:
--   Like break, but works with a [a] match.
--
-- > breakOnGlue ", " "one, two, three" ===> ("one", ", two, three")
breakOnGlue :: (Eq a) => [a] -- ^ Glue that holds pieces together
            -> [a]           -- ^ List from which to break off a piece
            -> ([a],[a])     -- ^ Result: (first piece, glue ++ rest of list)
breakOnGlue _ [] = ([],[])
breakOnGlue glue rest@(x:xs)
    | glue `isPrefixOf` rest = ([], rest)
    | otherwise = (x:piece, rest')
        where (piece, rest') = breakOnGlue glue xs
{-# INLINE breakOnGlue #-}

------------------------------------------------------------------------

drawUptime :: ClockTime -> ClockTime -> P.FastString
drawUptime before now =
    let r      = diffClockTimes now before
        s      = tdSec  r
        (h,sr) = quotRem s (60 * 60)
        m      = quot sr 60
    in printfPS fmt h m
  where
    fmt = P.pack "%3d:%02d"
        
------------------------------------------------------------------------
-- | Repeat an action
-- Also known as `forever' in the Awkward squad paper
repeatM_ :: Monad m => m a -> m ()
repeatM_ a = a >> repeatM_ a
{-# SPECIALIZE repeatM_ :: IO a -> IO () #-}
{-# INLINE repeatM_ #-}

------------------------------------------------------------------------

-- | Convert a (newtyped) Posix Fd to an Int we can use in other places
fdToInt :: Fd -> Int
fdToInt (Fd cint) = fromIntegral cint

-- | Wrap a CPid as a System.Process.ProcessHandle
pid2phdl :: ProcessID -> ProcessHandle
pid2phdl pid = (ProcessHandle pid)

------------------------------------------------------------------------
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

