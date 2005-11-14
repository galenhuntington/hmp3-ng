-- 
-- Copyright (c) 2004-5 Don Stewart - http://www.cse.unsw.edu.au/~dons
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

--
-- | Keymap manipulation
--
module Keymap where

import Lexers
import Core
import Curses
import State

import Data.List
import Control.Monad

import qualified Data.FastPackedString as P
import qualified Data.Map as M

--
-- The keymap
--
keymap :: [Char] -> [IO ()]
keymap cs = map (clrmsg >>) actions
    where (actions,_,_) = execLexer commands (cs, ()) 
 
commands :: Lexer () (IO ())
commands = (alt keys) `action` \[c] -> Just $ case M.lookup c keyMap of
        Nothing -> return ()    -- ignore
        Just a  -> a

--
-- The default keymap, and its description
--
keyTable :: [(P.FastString, [Char], IO ())]
keyTable =
    [(p "Previous track"#,         
        ['k',keyUp],    up)
    ,(p "Next track"#,             
        ['j',keyDown],  down)
    ,(p "Jump up"#,                
        [keyPPage],     replicateM 20 up   >> return ())
    ,(p "Jump down"#,              
        [keyNPage],     replicateM 20 down >> return ())
    ,(p "Seek left within song"#,  
        ['h',keyLeft],  seekLeft)
    ,(p "Seek right within song"#, 
        ['l',keyRight], seekRight)
    ,(p "Toggle pause"#,           
        ['p'],          pause)
    ,(p "Play song under cursor"#, 
        ['\n',' '],     play)
    ,(p "Toggle the help screen"#, 
        ['h'],          toggleHelp)
    ,(p "Jump to currently playing song"#, 
        ['c'],   jumpToPlaying)
    ,(p "Quit (or close help screen)"#, 
        ['q'],   do b <- helpIsVisible ; if b then toggleHelp else quit)
    ,(p "Select and play next track"#, 
        ['n'],   down >> play)
    ]
    where
        p = P.packAddress

helpIsVisible :: IO Bool
helpIsVisible = modifyState $ \st -> return (st, helpVisible st)

keyMap :: M.Map Char (IO ())
keyMap = M.fromList [ (c,a) | (_,cs,a) <- keyTable, c <- cs ]

keys :: [Char]
keys = concat [ cs | (_,cs,_) <- keyTable ]

------------------------------------------------------------------------
-- 
-- +       Increase volume for current song
-- -       Decrease volume for current song
-- n       Next song
-- /       Search within the playlist
-- A       Sort playlist according to Artist
-- S       Sort playlist according to Song title
-- T       Sort playlist according to Time
-- R       Sort playlist according to Rating
-- 1-9     Set rating of selected song
-- a       Add files to playlist
-- r       Enable/disable random play mode
-- l       Enable/disable loop play mode
-- e       Edit ID3 tags for selected song
-- s       Save playlist
