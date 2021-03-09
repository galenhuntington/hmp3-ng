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
import Lexers       ((>||<),action,meta,execLexer
                    ,alt,with,char,Regexp,Lexer)

import UI.HSCurses.Curses (Key(..), decodeKey)

import qualified Data.ByteString.Char8 as P
import qualified Data.Map as M
import Data.Bifunctor (first)

data Direction = Forwards | Backwards
    deriving stock Eq
-- data Zipper = Zipper { back :: [String], front :: [String], cur :: String }
type Zipper = (String, ([String], [String]))
data SearchType = SearchFile | SearchDir
data SearchSpec = SearchSpec
    { searchChar      :: !Char
    , searchType      :: !SearchType
    , searchDirection :: !Direction
    , searchZipper    :: Zipper
    }
data SearchState = SearchState
    { searchHist :: ![String]
    , searchSpec :: SearchSpec
    }


type LexerS = Lexer SearchState (IO ())
type MetaTarget = (Maybe (Either String (IO ())), SearchState, Maybe LexerS)

--
-- The keymap
--
keymap :: [Char] -> [IO ()]
keymap cs = map (clrmsg *>) actions
    where (actions,_,_) = execLexer all (cs, defaultS)

defaultS :: SearchState
defaultS = SearchState [] undefined

all :: LexerS
all = commands >||< search

commands :: LexerS
commands = alt keys `action` \[c] -> Just $ fromMaybe (pure ()) $ M.lookup c keyMap

------------------------------------------------------------------------

search :: LexerS
search = searchDir >||< searchFile

searchStart :: Char -> SearchType -> Direction -> LexerS
searchStart c typ dir = char c `meta` \_ (SearchState hist _) ->
    (with (toggleFocus *> putmsg (Fast (P.singleton c) defaultSty) *> touchST)
    , SearchState hist $ SearchSpec c typ dir ("", (hist, []))
    , Just dosearch)

searchDir :: LexerS
searchDir =  searchStart '\\' SearchDir Forwards
        >||< searchStart '|' SearchDir Backwards

searchFile :: LexerS
searchFile = searchStart '/' SearchFile Forwards
        >||< searchStart '?' SearchFile Backwards

dosearch :: LexerS
dosearch = search_char >||< search_bs >||< search_up >||< search_down >||< search_esc >||< search_eval

endSearchWith :: IO () -> [String] -> MetaTarget
endSearchWith a hist = (with (a *> toggleFocus), SearchState hist undefined, Just all)

zipEdit :: (String -> String) -> Zipper -> Zipper
zipEdit = first

zipTop :: Zipper -> String
zipTop = fst

printSearch :: SearchSpec -> Maybe (Either a (IO ()))
printSearch spec = with do
    putmsg $ Fast (P.pack $ searchChar spec : zipTop (searchZipper spec)) defaultSty
    touchST

updateSearch :: (Zipper -> Zipper) -> SearchState -> MetaTarget
updateSearch f (SearchState hist spec) =
    let spec' = spec{ searchZipper = f $ searchZipper spec }
    in (printSearch spec', SearchState hist spec', Just dosearch)

search_char :: LexerS
search_char = anyNonSpecial `meta` \c -> updateSearch $ zipEdit (++ c)
    where anyNonSpecial = alt $ any' \\ (enter' ++ delete' ++ ['\ESC'])

search_bs :: LexerS
search_bs = delete `meta`
    \_ -> updateSearch $ zipEdit \st -> case st of [] -> []; xs -> init xs

search_up :: LexerS
search_up = char (unkey KeyUp) `meta` \_ -> updateSearch \case
    (cur, (nx : rest, front)) -> (nx, (rest, cur : front))
    zipp                      -> zipp

search_down :: LexerS
search_down = char (unkey KeyDown) `meta` \_ -> updateSearch \case
    (cur, (back, pv : rest)) -> (pv, (cur : back, rest))
    zipp                     -> zipp

-- escape exits ex mode immediately
search_esc :: LexerS
search_esc = char '\ESC'
    `meta` \_ (SearchState hist _) -> endSearchWith (clrmsg *> touchST) hist

search_eval :: LexerS
search_eval = enter
    `meta` \_ (SearchState hist spec) -> case zipTop $ searchZipper spec of
        []  -> endSearchWith (clrmsg *> touchST) hist
        pat -> let jumpTo = case searchType spec of
                    SearchFile -> jumpToMatchFile
                    SearchDir  -> jumpToMatch
               in endSearchWith (jumpTo (Just pat) (searchDirection spec == Forwards))
                    $ if take 1 hist == [pat] then hist else pat : hist


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
