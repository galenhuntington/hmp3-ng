-- 
-- Copyright (c) 2003-2008 Don Stewart - http://www.cse.unsw.edu.au/~dons
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

-- miscellaneous utilites

module Utils where

import Base

import qualified Data.ByteString       as P (ByteString)
import qualified Data.ByteString.Char8 as P (pack)

import System.Clock             (TimeSpec(..), diffTimeSpec)

import Control.Monad.Fail as Fail  -- workaround needed for GHC 8.6


------------------------------------------------------------------------

--
-- | join two path components
--
infixr 6 </>
infixr 6 <+>

(</>), (<+>) :: FilePath -> FilePath -> FilePath
[] </> b = b
a  </> b = a ++ "/" ++ b

[] <+> b = b
a  <+> b = a ++ " " ++ b

------------------------------------------------------------------------

drawUptime :: TimeSpec -> TimeSpec -> P.ByteString
drawUptime before now
    | hs == 0 = P.pack $ printf "%dm" m
    | d == 0  = P.pack $ printf "%dh%02dm" h m
    | True    = P.pack $ printf "%dd%02dh%02dm" d h m
    where
        s      = sec $ diffTimeSpec now before
        ms     = quot s 60
        (hs,m) = quotRem ms 60
        (d,h)  = quotRem hs 24

------------------------------------------------------------------------

--
-- | Some evil to work out if the background is light, or dark. Assume dark.
--
isLightBg :: IO Bool
isLightBg = handle @SomeException (\_ -> pure False) do
    e <- getEnv "HMP_HAS_LIGHT_BG"
    return $ map toLower e == "true"

------------------------------------------------------------------------

-- | 'readM' behaves like read, but catches failure in a monad.
readM :: (MonadFail m, Read a) => String -> m a
readM s = case [x | (x,t) <- {-# SCC "Serial.readM.reads" #-} reads s    -- bad!
               , ("","")  <- lex t] of
        [x] -> return x
        []  -> Fail.fail "Serial.readM: no parse"
        _   -> Fail.fail "Serial.readM: ambiguous parse"

