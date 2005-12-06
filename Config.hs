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

#include "config.h"

defaultStyle :: UIStyle
defaultStyle  = UIStyle { window     = Style defaultfg    defaultbg
                        , titlebar   = Style brightwhite  green
                        , selected   = Style blue         defaultbg
                        , cursors    = Style black        cyan
                        , combined   = Style brightwhite  cyan
                        , warnings   = Style red          defaultbg
                        , helpscreen = Style black        white 
                        , blockcursor= Style black        red
                        , progress   = Style cyan         white }

-- | A style more suitable for light backgrounds (used if HMP_HAS_LIGHT_BG=true)
lightBgStyle :: UIStyle
lightBgStyle = 
           defaultStyle { selected   = Style darkblue     defaultbg
                        , warnings   = Style darkred      defaultbg }

--
-- | Another style for dark backgrounds, reminiscent of mutt
--
muttStyle :: UIStyle
muttStyle   = UIStyle { window     = Style brightwhite  black
                      , titlebar   = Style green        blue
                      , selected   = Style brightwhite  black
                      , cursors    = Style black        cyan
                      , combined   = Style black        cyan
                      , warnings   = Style brightwhite  red
                      , helpscreen = Style black        cyan
                      , blockcursor= Style black        darkred
                      , progress   = Style cyan         white  }

bwStyle :: UIStyle 
bwStyle = UIStyle {
        window      = Style defaultfg   defaultbg
       ,titlebar    = Style reversefg   reversebg
       ,selected    = Style brightwhite defaultbg
       ,cursors     = Style reversefg   reversebg
       ,combined    = Style reversefg   reversebg
       ,warnings    = Style reversefg   reversebg
       ,helpscreen  = Style reversefg   reversebg
       ,blockcursor = Style reversefg   reversebg
       ,progress    = Style reversefg   reversebg
    }
           
------------------------------------------------------------------------

package :: String
package = "hmp3"

versinfo :: String
versinfo  = package++" "++ version
    where 
      version :: String
      version = "0.2" ++ if (not . null) (PATCH_COUNT :: String) && (PATCH_COUNT /= "1")
                         then "p" ++ PATCH_COUNT
                         else ""

help :: String
help = "- curses-based MP3 player"

darcsinfo :: String
darcsinfo = "darcs get --partial http://www.cse.unsw.edu.au/~dons/code/hmp3"

