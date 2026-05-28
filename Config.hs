-- Copyright (c) 2005-2008 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (c) 2019-2021 Galen Huntington
-- SPDX-License-Identifier: GPL-2.0-or-later

module Config where

import Base
import Style
import Paths_hmp3_ng (version)

defaultStyle :: UIStyle
defaultStyle  = UIStyle { window     = Style defaultfg    defaultbg
                        , titlebar   = Style brightwhite  green
                        , selected   = Style blue         defaultbg
                        , cursors    = Style black        cyan
                        , combined   = Style brightwhite  cyan
                        , warnings   = Style red          defaultbg
                        , modals     = Style black        white
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
                      , modals     = Style black        cyan
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
       ,modals      = Style reversefg   reversebg
       ,blockcursor = Style reversefg   reversebg
       ,progress    = Style reversefg   reversebg
    }

------------------------------------------------------------------------

package :: String
package = "hmp3-ng"

versinfo :: String
versinfo  = package ++ " v" ++ showVersion version

