-- 
-- Copyright (c) 2005 Don Stewart - http://www.cse.unsw.edu.au/~dons
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

-- miscellaneous utilites

module Utils where

import Data.Char                (isSpace)
import Data.List                (isPrefixOf)

import qualified Data.FastPackedString as P

import Text.Printf              (printf)
import System.Time              (diffClockTimes, TimeDiff(tdSec), ClockTime)
import System.IO                (IO, FilePath)
import System.Posix.Types       (Fd(..),ProcessID)
import System.Process.Internals (ProcessHandle(..))

------------------------------------------------------------------------

-- some filename manipulation stuff

--
-- | </>, <.> : join two path components
--
infixr 6 </>
infixr 6 <.>

(</>), (<.>), (<>) :: FilePath -> FilePath -> FilePath
[] </> b = b
a  </> b = a ++ "/" ++ b

[] <.> b = b
a  <.> b = a ++ "." ++ b

[] <> b = b
a  <> b = a ++ b

basename :: FilePath -> FilePath
basename p = reverse $ takeWhile (/= '/') $ reverse p

-- | Packed string version of basename
basenameP :: P.FastString -> P.FastString
basenameP fps = case P.elemIndexLast '/' fps of
    Nothing -> fps
    Just i  -> P.drop (i+1) fps

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
    let r = diffClockTimes now before
        s = tdSec  r
        (h,sr) = quotRem s (60 * 60)
        m     = quot sr 60
    in P.pack $! ((printf "%3d:%02d" (h::Int) (m::Int)) :: String)
        
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

