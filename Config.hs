-- Copyright (c) 2005-2008 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (c) 2019-2021 Galen Huntington
-- SPDX-License-Identifier: GPL-2.0-or-later

module Config where

import Base
import Style
import Paths_hmp3_ng (version)

defaultStyle :: UIStyle
defaultStyle  = UIStyle { window     = style "default"      "default"
                        , titlebar   = style "brightwhite"  "green"
                        , selected   = style "blue"         "default"
                        , cursors    = style "black"        "cyan"
                        , combined   = style "brightwhite"  "cyan"
                        , warnings   = style "red"          "default"
                        , modals     = style "black"        "white"
                        , blockcursor= style "black"        "red"
                        , progress   = style "cyan"         "white" }

-- | A style more suitable for light backgrounds (used if HMP_HAS_LIGHT_BG=true)
lightBgStyle :: UIStyle
lightBgStyle =
           defaultStyle { selected   = style "darkblue"     "default"
                        , warnings   = style "darkred"      "default" }

--
-- | Another style for dark backgrounds, reminiscent of mutt
--
muttStyle :: UIStyle
muttStyle = UIStyle { window     = style "brightwhite"  "black"
                    , titlebar   = style "green"        "blue"
                    , selected   = style "brightwhite"  "black"
                    , cursors    = style "black"        "cyan"
                    , combined   = style "black"        "cyan"
                    , warnings   = style "brightwhite"  "red"
                    , modals     = style "black"        "cyan"
                    , blockcursor= style "black"        "darkred"
                    , progress   = style "cyan"         "white"  }

bwStyle :: UIStyle
bwStyle = UIStyle {
        window      = style "default"     "default"
       ,titlebar    = style "reverse"     "reverse"
       ,selected    = style "brightwhite" "default"
       ,cursors     = style "reverse"     "reverse"
       ,combined    = style "reverse"     "reverse"
       ,warnings    = style "reverse"     "reverse"
       ,modals      = style "reverse"     "reverse"
       ,blockcursor = style "reverse"     "reverse"
       ,progress    = style "reverse"     "reverse"
    }

------------------------------------------------------------------------

package :: String
package = "hmp3-ng"

versinfo :: String
versinfo  = package ++ " v" ++ showVersion version

help :: String
help = "- curses-based MP3 player"

