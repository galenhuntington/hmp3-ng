--
-- Copyright (c) 2020 Galen Huntington
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

module Base (module Prelude, module X) where

import Prelude

--  As of now, just including as needed.
--  I'm using the list in base-prelude as an upper bound on what qualifies.

import Control.Concurrent as X
import Control.Exception as X
import Control.Monad as X
import Data.Char as X
import Data.Foldable as X
import Data.Functor as X
import Data.IORef as X
import Data.List as X ((\\), group, groupBy, isPrefixOf, sort, sortBy, intersperse)
import Data.Maybe as X
import Data.Traversable as X
import Data.Word as X
import System.Environment as X
import System.Exit as X
import System.IO as X (Handle, hClose)
import Text.Printf as X

