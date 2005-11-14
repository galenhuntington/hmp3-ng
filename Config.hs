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
module Config where

import Style
import {-# SOURCE #-} qualified Keymap as Default

#include "config.h"

data Config = Config {
        keymap :: [Char] -> [IO ()],    -- ^ user-configurable keymap
        style  :: UIStyle               -- ^ colours
     }

-- default instance
config :: Config
config = Config {
       keymap = Default.keymap,
       style  = UIStyle { window     = Style Default Default
                        , highlight  = Style brightWhite green
                        , selected   = Style blue      Default
                        , cursors    = Style Default      cyan
                        , combined   = Style brightWhite  cyan
                        , warnings   = Style brightWhite  red
                        , helpscreen = Style Default cyan
                        , progress   = Style cyan    brightWhite  }
   }

------------------------------------------------------------------------

package :: String
package = "hmp3"

versinfo :: String
versinfo  = package++" "++ version++"p"++(show (PATCH_COUNT :: Int))
        where version :: String
              version = "0.0"

darcsinfo :: String
darcsinfo = "darcs get "++ REPO_PATH
