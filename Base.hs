module Base (module Prelude, module X) where

import Prelude hiding (fail)

--  As of now, just including as needed.
--  I'm using the list in base-prelude as an upper bound on what qualifies.
--  The fail gymnastics is for GHC 8.6 compat.

import Control.Concurrent as X
import Control.Exception as X
import Control.Monad as X hiding (fail, MonadFail)
import Control.Monad.Fail as X (fail, MonadFail)
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

