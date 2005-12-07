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

import Core
import State        (getsST, touchST, HState(helpVisible))
import Style        (defaultSty, StringA(Fast))
import qualified UI (resetui)
import Curses       (keyEnd,keyPPage,keyNPage,keyBackspace,keyHome
                    ,keyRight,keyLeft,keyUp,keyDown)
import Lexers       ((>|<),(>||<),action,meta,execLexer
                    ,alt,with,char,Regexp,Lexer)

import Data.List    ((\\))

import qualified Data.FastPackedString as P (packAddress,FastString,pack)
import qualified Data.Map as M (fromList, lookup, Map)

type LexerS = Lexer (Bool,[Char]) (IO ())

--
-- The keymap
--
keymap :: [Char] -> [IO ()]
keymap cs = map (clrmsg >>) actions
    where (actions,_,_) = execLexer all (cs, (True,[])) 

all :: LexerS
all = commands >||< search
  
commands :: LexerS
commands = (alt keys) `action` \[c] -> Just $ case M.lookup c keyMap of
        Nothing -> return ()    -- ignore
        Just a  -> a

------------------------------------------------------------------------

search :: LexerS
search = (char '/' >|< char '?') `meta` \[c] _ -> 
                (with (toggleFocus >> putmsg (Fast (P.pack [c]) defaultSty) >> touchST)
                ,((c == '/'), [c]) ,Just dosearch)

dosearch :: LexerS
dosearch = search_char >||< search_edit >||< search_esc >||< search_eval

search_char :: LexerS
search_char = anyButDelNL
    `meta` \c (d,st) -> (with (putmsg (Fast (P.pack(st++c)) defaultSty) >> touchST), (d,st++c), Just dosearch)
    where
        anyButDelNL = alt $ any' \\ (enter' ++ delete' ++ ['\ESC'])

search_edit :: LexerS
search_edit = delete 
    `meta` \_ (d,st) -> 
        let st' = case st of 
                    [c] -> [c]
                    xs  -> init xs
        in (with (putmsg (Fast (P.pack st') defaultSty) >> touchST), (d,st'), Just dosearch)

-- escape exits ex mode immediately
search_esc :: LexerS
search_esc = char '\ESC'
    `meta` \_ _ -> (with (clrmsg >> touchST >> toggleFocus), (True,[]), Just all)

search_eval :: LexerS
search_eval = enter
    `meta` \_ (d,(_:pat)) -> case pat of
        [] -> (with (clrmsg >> touchST >> toggleFocus),     (True,[]), Just all)
        _  -> (with (jumpToMatch (Just (pat,d)) >> toggleFocus), (True,[]), Just all)

------------------------------------------------------------------------

enter', any', digit', delete' :: [Char]
enter'   = ['\n', '\r']
delete'  = ['\BS', '\127', keyBackspace ]
any'     = ['\0' .. '\255']
digit'   = ['0' .. '9']

delete, enter :: Regexp (Bool,[Char]) (IO ())
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
    ,(p "Next directory down"#,                
        [keyNPage], jumpToNextDir)
    ,(p "Next directory up"#,                
        [keyPPage], jumpToPrevDir)
    ,(p "Jump to start of list"#, 
        [keyHome],  jump 0)
    ,(p "Jump to end of list"#, 
        [keyEnd],   jump maxBound)
    ,(p "Seek left within song"#,  
        [keyLeft],  seekLeft)
    ,(p "Seek right within song"#, 
        [keyRight], seekRight)
    ,(p "Toggle pause"#,           
        ['p'],          pause)
    ,(p "Play song under cursor"#, 
        ['\n',' '],     play)
    ,(p "Toggle the help screen"#, 
        ['h'],          toggleHelp)
    ,(p "Jump to currently playing song"#, 
        ['t'],   jumpToPlaying)
    ,(p "Quit (or close help screen)"#, 
        ['q'],   do b <- helpIsVisible ; if b then toggleHelp else quit Nothing)
    ,(p "Select and play next track"#, 
        ['d'],   playNext >> jumpToPlaying)
    ,(p "Cycle through normal, random and loop modes"#,
        ['m'],   nextMode)
    ,(p "Refresh the display"#,
        ['\^L'], UI.resetui)
    ,(p "Repeat last regex search"#, 
        ['n'],   jumpToMatch Nothing)
    ]
  where
    -- Keep as Addr#. If we try the pack/packAddress rule, ghc seems to get
    -- confused and want to *unpack* the strings :/
    p = P.packAddress
    {-# INLINE p #-}

extraTable :: [(P.FastString, [Char])]
extraTable = [(p "Search for directory matching regex"#, ['/'])
             ,(p "Search backwards for directory"#, ['?'])]
  where
    -- Keep as Addr#. If we try the pack/packAddress rule, ghc seems to get
    -- confused and want to *unpack* the strings :/
    p = P.packAddress
    {-# INLINE p #-}


helpIsVisible :: IO Bool
helpIsVisible = getsST helpVisible

keyMap :: M.Map Char (IO ())
keyMap = M.fromList [ (c,a) | (_,cs,a) <- keyTable, c <- cs ]

keys :: [Char]
keys = concat [ cs | (_,cs,_) <- keyTable ]
