-- 
-- Copyright (c) 2004-2008 Don Stewart - http://www.cse.unsw.edu.au/~dons
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
-- The idea of using lazy lexers to implement keymaps is described in
-- the paper:
--
-- >  Dynamic Applications From the Ground Up. Don Stewart and Manuel M.
-- >  T. Chakravarty. In Proceedings of the ACM SIGPLAN Workshop on
-- >  Haskell, pages 27-38. ACM Press, 2005.
-- 
-- See that for more info.
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

import qualified Data.ByteString.Char8 as P (ByteString, pack)
import qualified Data.Map as M (fromList, lookup, Map)

data Search = SearchFile | SearchDir

data Direction = Forwards | Backwards

toBool :: Direction -> Bool
toBool Forwards = True
toBool _        = False

type LexerS = Lexer (Search, Direction, String) (IO ())

--
-- The keymap
--
keymap :: [Char] -> [IO ()]
keymap cs = map (clrmsg >>) actions
    where (actions,_,_) = execLexer all (cs, defaultS)

defaultS :: (Search, Direction, String)
defaultS = (SearchDir, Forwards, [])

all :: LexerS
all = commands >||< search

commands :: LexerS
commands = (alt keys) `action` \[c] -> Just $ case M.lookup c keyMap of
        Nothing -> return ()    -- ignore
        Just a  -> a

------------------------------------------------------------------------

search :: LexerS
search = searchDir >||< searchFile

searchDir :: LexerS
searchDir = (char '/' >|< char '?') `meta` \[c] _ ->
                (with (toggleFocus >> putmsg (Fast (P.pack [c]) defaultSty) >> touchST)
                ,(SearchDir,if c == '/' then Forwards else Backwards,[c]) ,Just dosearch)

searchFile :: LexerS
searchFile = (char '\\' >|< char '|') `meta` \[c] _ ->
                (with (toggleFocus >> putmsg (Fast (P.pack [c]) defaultSty) >> touchST)
                ,(SearchFile,if c == '\\' then Forwards else Backwards,[c]) ,Just dosearch)

dosearch :: LexerS
dosearch = search_char >||< search_edit >||< search_esc >||< search_eval

search_char :: LexerS
search_char = anyButDelNL
    `meta` \c (t,d,st) ->
        (with (putmsg (Fast (P.pack(st++c)) defaultSty) >> touchST), (t,d,st++c), Just dosearch)
    where anyButDelNL = alt $ any' \\ (enter' ++ delete' ++ ['\ESC'])

search_edit :: LexerS
search_edit = delete
    `meta` \_ (t,d,st) ->
        let st' = case st of [c] -> [c]; xs  -> init xs
        in (with (putmsg (Fast (P.pack st') defaultSty) >> touchST), (t,d,st'), Just dosearch)

-- escape exits ex mode immediately
search_esc :: LexerS
search_esc = char '\ESC'
    `meta` \_ _ -> wrap (clrmsg >> touchST)
    where wrap a = (with (a >> toggleFocus), defaultS, Just all)

search_eval :: LexerS
search_eval = enter
    `meta` \_ (t,d,(_:pat)) -> case pat of
        [] -> wrap (clrmsg >> touchST)
        _  -> case t of
                SearchFile -> wrap (jumpToMatchFile (Just (pat,toBool d)))
                SearchDir  -> wrap (jumpToMatch     (Just (pat,toBool d)))

    where wrap a = (with (a >> toggleFocus), defaultS, Just all)

------------------------------------------------------------------------

enter', any', digit', delete' :: [Char]
enter'   = ['\n', '\r']
delete'  = ['\BS', '\127', keyBackspace ]
any'     = ['\0' .. '\255']
digit'   = ['0' .. '9']

delete, enter :: Regexp (Search,Direction,[Char]) (IO ())
delete  = alt delete'
enter   = alt enter'

------------------------------------------------------------------------

--
-- The default keymap, and its description
--
keyTable :: [(P.ByteString, [Char], IO ())]
keyTable =
    [
     (p "Move up",
        ['k',keyUp],    up)
    ,(p "Move down",
        ['j',keyDown],  down)
    ,(p "Next directory down",
        [keyNPage], jumpToNextDir)
    ,(p "Next directory up",
        [keyPPage], jumpToPrevDir)
    ,(p "Jump to start of list",
        [keyHome,'1'],  jump 0)
    ,(p "Jump to end of list",
        [keyEnd,'G'],   jump maxBound)
    ,(p "Seek left within song",
        [keyLeft],  seekLeft)
    ,(p "Seek right within song",
        [keyRight], seekRight)
    ,(p "Toggle pause",
        ['p'],          pause)
    ,(p "Play song under cursor",
        ['\n',' '],     play)
    ,(p "Play previous track",
        ['K'],    playPrev)
    ,(p "Play next track",
        ['J'],  playNext)
    ,(p "Toggle the help screen",
        ['h'],   toggleHelp)
    ,(p "Jump to currently playing song",
        ['t'],   jumpToPlaying)
    ,(p "Quit (or close help screen)",
        ['q'],   do b <- helpIsVisible ; if b then toggleHelp else quit Nothing)
    ,(p "Select and play next track",
        ['d'],   playNext >> jumpToPlaying)
    ,(p "Cycle through normal, random and loop modes",
        ['m'],   nextMode)
    ,(p "Refresh the display",
        ['\^L'], UI.resetui)
    ,(p "Repeat last regex search",
        ['n'],   jumpToMatch Nothing)
    ,(p "Repeat last regex search on files",
        ['N'],   jumpToMatchFile Nothing)
    ,(p "Load config file",
        ['l'],   loadConfig)
    ]
  where
    -- Keep as Addr#. If we try the pack/packAddress rule, ghc seems to get
    -- confused and want to *unpack* the strings :/
    p = P.pack
    {-# INLINE p #-}

extraTable :: [(P.ByteString, [Char])]
extraTable = [(p "Search for directory matching regex", ['/'])
             ,(p "Search backwards for directory", ['?'])
             ,(p "Search for file matching regex", ['\\'])
             ,(p "Search backwards for file", ['|']) ]
  where
    -- Keep as Addr#. If we try the pack/packAddress rule, ghc seems to get
    -- confused and want to *unpack* the strings :/
    p = P.pack
    {-# INLINE p #-}


helpIsVisible :: IO Bool
helpIsVisible = getsST helpVisible

keyMap :: M.Map Char (IO ())
keyMap = M.fromList [ (c,a) | (_,cs,a) <- keyTable, c <- cs ]

keys :: [Char]
keys = concat [ cs | (_,cs,_) <- keyTable ]
