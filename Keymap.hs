-- 
-- Copyright (c) 2004-2008 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (c) 2008, 2019-2021 Galen Huntington
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
import State        (getsST, touchST, HState(helpVisible, playHist))
import Style        (defaultSty, StringA(Fast))
import qualified UI (resetui)
import Lexers       ((>||<),action,meta,execLexer
                    ,alt,with,char,Regexp,Lexer)

import UI.HSCurses.Curses (Key(..), decodeKey)

import qualified Data.ByteString.Char8 as P
import qualified Data.Map as M
import qualified Data.Sequence as Seq

data Direction = Forwards | Backwards
data Zipper = Zipper { cur :: !String, back :: ![String], front :: ![String] }
data SearchWhat = SearchFiles | SearchDirs
data SearchType = SearchType
    { schChar :: !Char
    , schWhat :: !SearchWhat
    , schDir  :: !Direction
    }
data SearchSpec = SearchSpec
    { schType   :: !SearchType
    , schZipper :: !Zipper
    }
data SearchState = SearchState
    { schHist :: ![String]
    , schSpec :: SearchSpec
    }

type LexerS = Lexer SearchState (IO ())
type Result = Maybe (Either String (IO ()))
type MetaTarget = (Result, SearchState, Maybe LexerS)

--
-- The keymap
--
keymap :: [Char] -> [IO ()]
keymap cs = map (clrmsg *>) actions
    where (actions,_,_) = execLexer all (cs, SearchState [] undefined)

all :: LexerS
all = commands >||< search >||< history

commands :: LexerS
commands = alt keys `action` \[c] -> Just $ fromMaybe (pure ()) $ M.lookup c keyMap

------------------------------------------------------------------------

search :: LexerS
search = searchDirs >||< searchFiles

searchStart :: Char -> SearchWhat -> Direction -> LexerS
searchStart c typ dir = char c `meta` \_ (SearchState hist _) ->
    (with (toggleFocus *> putmsg (Fast (P.singleton c) defaultSty) *> touchST)
    , SearchState hist $ SearchSpec (SearchType c typ dir) (Zipper "" hist [])
    , Just dosearch)

searchDirs :: LexerS
searchDirs =  searchStart '\\' SearchDirs Forwards
        >||< searchStart '|' SearchDirs Backwards

searchFiles :: LexerS
searchFiles = searchStart '/' SearchFiles Forwards
        >||< searchStart '?' SearchFiles Backwards

dosearch :: LexerS
dosearch = search_char >||< search_bs >||< search_up >||< search_down >||< search_esc >||< search_eval

endSearchWith :: IO () -> [String] -> MetaTarget
endSearchWith a hist = (with (a *> toggleFocus), SearchState hist undefined, Just all)

-- "lens"
zipEdit :: (String -> String) -> Zipper -> Zipper
zipEdit f zipp = zipp{cur = f $ cur zipp}

printSearch :: SearchSpec -> Maybe (Either a (IO ()))
printSearch spec = with do
    putmsg $ Fast (P.pack $ schChar (schType spec) : cur (schZipper spec)) defaultSty
    touchST

updateSearch :: (Zipper -> Zipper) -> SearchState -> MetaTarget
updateSearch f st@(SearchState _ spec) =
    let spec' = spec{ schZipper = f $ schZipper spec }
    in (printSearch spec', st{schSpec=spec'}, Just dosearch)

search_char :: LexerS
search_char = anyNonSpecial `meta` \c -> updateSearch $ zipEdit (++ c)
    where anyNonSpecial = alt $ any' \\ (enter' ++ delete' ++ ['\ESC'])

search_bs :: LexerS
search_bs = delete `meta`
    \_ -> updateSearch $ zipEdit \case [] -> []; xs -> init xs

search_up :: LexerS
search_up = char (unkey KeyUp) `meta` \_ -> updateSearch \case
    Zipper cur (nx:rest) front -> Zipper nx rest (cur:front)
    zipp                       -> zipp

search_down :: LexerS
search_down = char (unkey KeyDown) `meta` \_ -> updateSearch \case
    Zipper cur back (pv:rest) -> Zipper pv (cur:back) rest
    zipp                      -> zipp

search_esc :: LexerS
search_esc = char '\ESC' `meta`
    \_ (SearchState hist _) -> endSearchWith (clrmsg *> touchST) hist

search_eval :: LexerS
search_eval = enter `meta` \_ (SearchState hist spec) -> case cur $ schZipper spec of
    []  -> endSearchWith (clrmsg *> touchST) hist
    pat ->
        let typ = schType spec
            jumpy = case schWhat typ of
              SearchFiles -> jumpToMatchFile
              SearchDirs  -> jumpToMatch
        in endSearchWith
            do jumpy (Just pat) case schDir typ of Forwards -> True; _ -> False
            do if take 1 hist == [pat] then hist else pat : hist


------------------------------------------------------------------------

history :: LexerS
history = alt ['H', '.'] `meta`
        \_ st -> (with (showHist *> touchST), st, Just inner) where
    inner =
        alt any' `meta` (\_ st -> (with (hideHist *> touchST), st, Just all))
        >||< alt ['0'..'9'] `meta` handleKey '0' 0
        >||< alt ['a'..'z'] `meta` handleKey 'a' 10
    handleKey base off cs st =
        (with do
            ph <- getsST playHist
            whenJust
                do ph Seq.!? (fromEnum (head cs) - (fromEnum base - off))
                do jump . snd
            hideHist
            touchST
        , st
        , Just all
        )

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

delete, enter :: Regexp SearchState (IO ())
delete  = alt delete'
enter   = alt enter'

------------------------------------------------------------------------

--
-- The default keymap, and its description
--
keyTable :: [(String, [Char], IO ())]
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
    ,("Mark for deletion in ~/.hmp3-delete",
        ['D'],   blacklist)
    ,("Load config file",
        ['l'],   loadConfig)
    ,("Restart song",
        [unkey KeyBackspace],   seekStart)
    ]

innerTable :: [(Char, IO ())]
innerTable = [(c, jumpRel i) | (i, c) <- zip [0.1, 0.2 ..] ['1'..'9']]

extraTable :: [(String, [Char])]
extraTable = [("Toggle the song history", ['H', '.'])
             ,("Search for file matching regex", ['/'])
             ,("Search backwards for file", ['?'])
             ,("Search for directory matching regex", ['\\'])
             ,("Search backwards for directory", ['|']) ]

helpIsVisible :: IO Bool
helpIsVisible = getsST helpVisible

keyMap :: M.Map Char (IO ())
keyMap = M.fromList $ [ (c,a) | (_,cs,a) <- keyTable, c <- cs ] ++ innerTable

keys :: [Char]
keys = concat [ cs | (_,cs,_) <- keyTable ] ++ map fst innerTable
