-- 
-- Copyright (c) 2004-2008 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (c) 2008, 2019, 2020 Galen Huntington
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

import Prelude ()
import Base hiding (all)

import Core
import State        (getsST, touchST, HState(helpVisible))
import Style        (defaultSty, StringA(Fast))
import qualified UI (resetui)
import Lexers       ((>|<),(>||<),action,meta,execLexer
                    ,alt,with,char,Regexp,Lexer)

import UI.HSCurses.Curses (Key(..), decodeKey)

import qualified Data.ByteString.Char8 as P
import qualified Data.Map as M

data Search = SearchFile | SearchDir

data Direction = Forwards | Backwards

toBool :: Direction -> Bool
toBool Forwards = True
toBool _        = False

type Context = (Search, Direction, String)

type LexerS = Lexer Context (IO ())

--
-- The keymap
--
keymap :: [Char] -> [IO ()]
keymap cs = map (clrmsg *>) actions
    where (actions,_,_) = execLexer all (cs, defaultS)

defaultS :: Context
defaultS = (SearchDir, Forwards, [])

all :: LexerS
all = commands >||< search

commands :: LexerS
commands = alt keys `action` \[c] -> Just $ fromMaybe (pure ()) $ M.lookup c keyMap

------------------------------------------------------------------------

search :: LexerS
search = searchDir >||< searchFile

searchDir :: LexerS
searchDir = (char '\\' >|< char '|') `meta` \[c] _ ->
                (with (toggleFocus *> putmsg (Fast (P.singleton c) defaultSty) *> touchST)
                ,(SearchDir,if c == '\\' then Forwards else Backwards,[c]) ,Just dosearch)

searchFile :: LexerS
searchFile = (char '/' >|< char '?') `meta` \[c] _ ->
                (with (toggleFocus *> putmsg (Fast (P.singleton c) defaultSty) *> touchST)
                ,(SearchFile,if c == '/' then Forwards else Backwards,[c]) ,Just dosearch)

dosearch :: LexerS
dosearch = search_char >||< search_edit >||< search_esc >||< search_eval

endSearchWith :: IO () -> (Maybe (Either err (IO ())), Context, Maybe LexerS)
endSearchWith a = (with (a *> toggleFocus), defaultS, Just all)

search_char :: LexerS
search_char = anyNonSpecial
    `meta` \c (t,d,st) ->
        (with (putmsg (Fast (P.pack(st++c)) defaultSty) *> touchST), (t,d,st++c), Just dosearch)
    where anyNonSpecial = alt $ any' \\ (enter' ++ delete' ++ ['\ESC'])

search_edit :: LexerS
search_edit = delete
    `meta` \_ (t,d,st) ->
        let st' = case st of [c] -> [c]; xs  -> init xs
        in (with (putmsg (Fast (P.pack st') defaultSty) *> touchST), (t,d,st'), Just dosearch)

-- escape exits ex mode immediately
search_esc :: LexerS
search_esc = char '\ESC'
    `meta` \_ _ -> endSearchWith (clrmsg *> touchST)

search_eval :: LexerS
search_eval = enter
    `meta` \_ (t, d, _:pat) -> case pat of
        [] -> endSearchWith (clrmsg *> touchST)
        _  -> let jumpTo = case t of
                    SearchFile -> jumpToMatchFile
                    SearchDir  -> jumpToMatch
              in endSearchWith (jumpTo (Just pat) (toBool d))

------------------------------------------------------------------------

-- "Key"s seem to be inscrutable and incomparable.
-- Solution is to translate to chars.  Really hacky!
--   TODO at least use lookup table (standalone deriving Ord)
unkey :: Key -> Char
unkey k = let Just c' = find (\c -> charToKey c == k) ['\0' .. '\500'] in c'

charToKey :: Char -> Key
charToKey = decodeKey . toEnum . fromEnum

enter', any', digit', delete' :: [Char]
enter'   = ['\n', '\r']
delete'  = ['\BS', '\127', unkey KeyBackspace]
any'     = ['\0' .. '\255']
digit'   = ['0' .. '9']

delete, enter :: Regexp (Search,Direction,[Char]) (IO ())
delete  = alt delete'
enter   = alt enter'

------------------------------------------------------------------------

--
-- The default keymap, and its description
--
keyTable :: [(ByteString, [Char], IO ())]
keyTable =
    [
     ("Move up",
        ['k',unkey KeyUp],    up)
    ,("Move down",
        ['j',unkey KeyDown],  down)
    ,("Page down",
        [unkey KeyNPage], downPage)
    ,("Page up",
        [unkey KeyPPage], upPage)
    ,("Jump to start of list",
        [unkey KeyHome,'0'],  jump 0)
    ,("Jump to end of list",
        [unkey KeyEnd,'G'],   jump maxBound)
    ,("Jump to 10%, 20%, 30%, etc., point",
        ['1','2','3'], undefined) -- overridden below
    ,("Seek left within song",
        [unkey KeyLeft],  seekLeft)
    ,("Seek right within song",
        [unkey KeyRight], seekRight)
    ,("Toggle pause",
        [' '],          pause)
    ,("Play song under cursor",
        ['\n'],     play)
    ,("Play previous track",
        ['K'],    playPrev)
    ,("Play next track",
        ['J'],  playNext)
    ,("Toggle the help screen",
        ['h'],   toggleHelp)
    ,("Jump to currently playing song",
        ['t'],   jumpToPlaying)
    ,("Quit (or close help screen)",
        ['q'],   do b <- helpIsVisible ; if b then toggleHelp else quit Nothing)
    ,("Select and play next track",
        ['d'],   playNext *> jumpToPlaying)
    ,("Cycle through normal, random, and loop modes",
        ['m'],   nextMode)
    ,("Refresh the display",
        ['\^L'], UI.resetui)
    ,("Repeat last regex search",
        ['n'],   jumpToMatchFile Nothing True)
    ,("Repeat last regex search backwards",
        ['N'],   jumpToMatchFile Nothing False)
    ,("Play",
        ['p'],   playCur)
    ,("Mark as deletable",
        ['d'],   blacklist)
    ,("Load config file",
        ['l'],   loadConfig)
    ,("Restart song",
        [unkey KeyBackspace],   seekStart)
    ]

innerTable :: [(Char, IO ())]
innerTable = [(c, jumpRel i) | (i, c) <- zip [0.1, 0.2 ..] ['1'..'9']]

extraTable :: [(ByteString, [Char])]
extraTable = [("Search for file matching regex", ['/'])
             ,("Search backwards for file", ['?'])
             ,("Search for directory matching regex", ['\\'])
             ,("Search backwards for directory", ['|']) ]

helpIsVisible :: IO Bool
helpIsVisible = getsST helpVisible

keyMap :: M.Map Char (IO ())
keyMap = M.fromList $ [ (c,a) | (_,cs,a) <- keyTable, c <- cs ] ++ innerTable

keys :: [Char]
keys = concat [ cs | (_,cs,_) <- keyTable ] ++ map fst innerTable
