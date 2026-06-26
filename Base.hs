-- Copyright (c) 2020-2026 Galen Huntington
-- SPDX-License-Identifier: GPL-2.0-or-later

module Base (module Prelude, module X, module Base) where

import Prelude

--  As of now, just including as needed.
--  I'm using the list in rebase as an upper bound on what qualifies.

import Control.Concurrent as X
import Control.Exception as X
import Control.Monad as X
import Data.ByteString as X (ByteString)
import Data.Char as X
import Data.Either as X
import Data.Fixed as X
import Data.Foldable as X
import Data.Functor as X hiding (unzip)
import Data.IORef as X
import Data.List as X hiding ((!?))
import Data.Maybe as X
import Data.Sequence as X (Seq, (<|), (|>))
import Data.String as X
import Data.Traversable as X
import Data.Version as X
import Data.Void as X
import Data.Word as X
import System.Exit as X
import System.IO as X (Handle, hClose)
import System.IO.Unsafe as X
import Text.Printf as X
import Text.Read as X (readMaybe)

import System.Clock


--  Random utility functions.

discardErrors :: IO () -> IO ()
discardErrors = X.handle @SomeException (\_ -> pure ())

getMonoTime :: IO TimeSpec
getMonoTime = getTime Monotonic

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust = flip $ maybe $ pure ()

-- Compatibility: List.!? only added in GHC 9.8
(!?) :: [a] -> Int -> Maybe a
xs !? n = listToMaybe $ drop n xs

-- | Zipper structure, representing a list with a cursor.
data Zipper a = Zipper { cur :: !a, back :: ![a], front :: ![a] }

zipEdit :: (a -> a) -> Zipper a -> Zipper a
zipEdit f z = z { cur = f z.cur }

zipUp, zipDown :: Zipper a -> Zipper a
zipUp   (Zipper c (nx:rest) f)  = Zipper nx rest (c:f)
zipUp   z                       = z
zipDown (Zipper c b (pv:rest))  = Zipper pv (c:b) rest
zipDown z                       = z

