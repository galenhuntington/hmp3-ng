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
import Text.Regex.Posix (match, makeRegexOptsM, compIgnoreCase, compExtended)

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


-- API for searching
matches :: ByteString -> ByteString -> Bool

-- Layer allowing switching back end

{-
-- pcre-light version (can't use currently due to pcre3 dep)
matches s = case compileM s [caseless, utf8] of
    Right p -> \t -> isJust $ match p t []
    _       -> const False
-}

{-
-- regex-pcre2 version (fails to build in CI, not in Stackage)
matches s = case makeRegexOptsM compCaseless 0 s of
    Just p -> match p
    _      -> const False
-}

{-
-- pcre2 version (inefficient, mass Text conversion, ugly)
-- needs text dep/import
matches s =
    let p = decodeUtf8Lenient s
    in \t -> unsafePerformIO
        $ handle @SomeException (const $ pure False) $ evaluate
        $ matchesOpt Caseless p (decodeUtf8Lenient t)
-}

-- regex-posix version (reputed to be slow and buggy)
matches s = maybe (const False) match $
    makeRegexOptsM (compIgnoreCase + compExtended) 0 s

-- not yet tried:
-- regex-tdfa (mass Text conversion, parsec dep) text import
-- regex-dfa (not in Stackage, unknown engine)

