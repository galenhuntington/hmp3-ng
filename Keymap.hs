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

import Prelude hiding (all)

import Lexers
import Core
import Curses
import State
import Style
import qualified UI

import Data.List        hiding (delete, all)
import Control.Monad

import qualified Data.FastPackedString as P
import qualified Data.Map as M

--
-- The keymap
--
keymap :: [Char] -> [IO ()]
keymap cs = map (clrmsg >>) actions
    where (actions,_,_) = execLexer all (cs, []) 

all :: Lexer [Char] (IO ())
all = commands >||< search
  
commands :: Lexer [Char] (IO ())
commands = (alt keys) `action` \[c] -> Just $ case M.lookup c keyMap of
        Nothing -> return ()    -- ignore
        Just a  -> a

------------------------------------------------------------------------

search :: Lexer [Char] (IO ())
search = char '/' `meta` \_ _ -> 
            (with (putmsg (Plain "/") >> touchState), ['/'], Just dosearch)

dosearch :: Lexer [Char] (IO ())
dosearch = search_char >||< search_edit >||< search_esc >||< search_eval

search_char :: Lexer [Char] (IO ())
search_char = anyButDelNL
    `meta` \c st -> (with (putmsg (Plain $ st++c) >> touchState), st++c, Just dosearch)
    where
        anyButDelNL = alt $ any' \\ (enter' ++ delete' ++ ['\ESC'])

search_edit :: Lexer [Char] (IO ())
search_edit = delete 
    `meta` \_ st -> 
    let st' = case st of 
                [c] -> [c]
                xs  -> init xs
    in (with (putmsg (Plain st') >> touchState), st', Just search_edit)

-- escape exits ex mode immediately
search_esc :: Lexer [Char] (IO ())
search_esc = char '\ESC'
    `meta` \_ _ -> (with (clrmsg >> touchState), [], Just all)

search_eval :: Lexer [Char] (IO ())
search_eval = enter
    `meta` \_ ('/':pat) -> (with (jumpToMatch pat), [], Just all)

------------------------------------------------------------------------

enter', any', digit', delete' :: [Char]
enter'   = ['\n', '\r']
delete'  = ['\BS', '\127', keyBackspace ]
any'     = ['\0' .. '\255']
digit'   = ['0' .. '9']

delete, enter :: Regexp [Char] (IO ())
delete  = alt delete'
enter   = alt enter'

------------------------------------------------------------------------

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
    ,(p "Jump to start of list"#, 
        [keyHome],  jump 0)
    ,(p "Jump to end of list"#, 
        [keyEnd],   jump maxBound)
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
        ['t'],   jumpToPlaying)
    ,(p "Quit (or close help screen)"#, 
        ['q'],   do b <- helpIsVisible ; if b then toggleHelp else quit)
    ,(p "Select and play next track"#, 
        ['n'],   playNext)
    ,(p "Cycle through normal, random and loop modes"#,
        ['m'],   nextMode)
    ,(p "Save the current playlist"#,
        ['w'],   writeSt)
    ,(p "Refresh the display"#,
        ['\^L'], UI.resetui)
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
-- /       Search within the playlist
-- A       Sort playlist according to Artist
-- S       Sort playlist according to Song title
-- T       Sort playlist according to Time
-- R       Sort playlist according to Rating
-- 1-9     Set rating of selected song
-- a       Add files to playlist
-- e       Edit ID3 tags for selected song
-- s       Save playlist
