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
import Data.List as X ((\\), group, groupBy, isPrefixOf, sort, sortBy)
import Data.Maybe as X
import Data.Traversable as X
import Data.Word as X
import System.Environment as X
import System.Exit as X
import System.IO as X (Handle, hClose)
import Text.Printf as X

