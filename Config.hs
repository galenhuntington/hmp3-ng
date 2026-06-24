-- Copyright (c) 2005-2008 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (c) 2019-2026 Galen Huntington
-- SPDX-License-Identifier: GPL-2.0-or-later

module Config where

import Base
import Paths_hmp3_ng (version)


package :: String
package = "hmp3-ng"

versinfo :: String
versinfo  = package ++ " v" ++ showVersion version

