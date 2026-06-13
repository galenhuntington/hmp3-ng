{-# OPTIONS_GHC -Wno-orphans #-}

-- Copyright (c) 2019, 2023-2026 Galen Huntington
-- SPDX-License-Identifier: GPL-2.0-or-later

module Keyboard (unkey, charToKey, Key(..)) where

import Base

import Data.Map.Strict qualified as M
import UI.HSCurses.Curses (Key(..), decodeKey)

------------------------------------------------------------------------
-- Char ↔ Key translation
--
-- ncurses delivers special keys as integer codes ≥ 256; for everything
-- in 0..255 'decodeKey' returns 'KeyChar (chr n)'.  We keep working in
-- 'Char' (UI.getKey's type), so we extend the range up to '\500' to
-- cover the named keys we actually use (KEY_RESIZE is around 410).

deriving stock instance Ord Key

charToKey :: Char -> Key
charToKey = decodeKey . toEnum . fromEnum

keyCharMap :: M.Map Key Char
keyCharMap = M.fromList [(charToKey c, c) | c <- ['\0' .. '\500']]

unkey :: Key -> Char
unkey k = fromMaybe '\0' $ M.lookup k keyCharMap

