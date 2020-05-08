{-# OPTIONS -fno-warn-orphans #-}
-- 
-- Copyright (c) 2005-8 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (c) 2019, 2020 Galen Huntington
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
-- functions for manipulating file trees
--

module Tree where

import Base

import FastIO
import Syntax           (Mode(..))
import qualified Data.ByteString.Char8 as P
import qualified Data.ByteString.Lazy  as L
import Codec.Compression.GZip

import Data.Binary
import Data.Array
import System.IO        (hPrint, stderr)


type FilePathP = P.ByteString

-- | A filesystem hierarchy is flattened to just the end nodes
type DirArray = Array Int Dir

-- | The complete list of .mp3 files
type FileArray = Array Int File

-- | A directory entry is the directory name, and a list of bound
-- indicies into the Files array.
data Dir  =
    Dir { dname :: !FilePathP        -- ^ directory name
        , dsize :: !Int              -- ^ number of file entries
        , dlo   :: !Int              -- ^ index of first entry
        , dhi   :: !Int }            -- ^ index of last entry

-- Most data is allocated in this structure
data File =
    File { fbase :: !FilePathP      -- ^ basename of file
         , fdir  :: !Int }          -- ^ index of Dir entry 

--
-- | Given the start directories, populate the dirs and files arrays
--
buildTree :: [FilePathP] -> IO (DirArray, FileArray)
buildTree fs = do
    (os,dirs) <- partition fs    -- note we will lose the ordering of files given on cmd line.

    let loop xs | seq xs False = undefined -- strictify
        loop []     = return []
        loop (a:xs) = do
            (m,ds) <- expandDir a
            ms     <- loop $! ds ++ xs  -- add to work list
            return $! m : ms

    ms' <- catMaybes <$!> loop dirs

    let extras = merge . doOrphans $ os
        ms = ms' ++ extras

    let (_,n,dirls,filels) = foldl' make (0,0,[],[]) ms
        dirsArray = listArray (0,length dirls - 1) (reverse dirls)
        fileArray = listArray (0, n-1) (reverse filels)

    dirsArray `seq` fileArray `seq` pure (dirsArray, fileArray)

-- | Create nodes based on dirname for orphan files on cmdline
doOrphans :: [FilePathP] -> [(FilePathP, [FilePathP])]
doOrphans = map (\f -> (dirnameP f, [basenameP f]))

-- | Merge entries with the same root node into a single node
merge :: [(FilePathP, [FilePathP])] -> [(FilePathP, [FilePathP])]
merge [] = []
merge xs =
    let xs' = sortBy  (\a b -> fst a `compare` fst b) xs
        xs''= groupBy (\a b -> fst a == fst b) xs'
    in mapMaybe flatten xs''
  where
    flatten :: [(FilePathP,[FilePathP])] -> Maybe (FilePathP, [FilePathP])
    flatten []     = Nothing    -- can't happen
    flatten (x:ys) = let d = fst x in Just (d, snd x ++ concatMap snd ys)

-- | fold builder, for generating Dirs and Files
make :: (Int,Int,[Dir],[File]) -> (FilePathP,[FilePathP]) -> (Int,Int,[Dir],[File])
make (i,n,acc1,acc2) (d,fs) =
    case listToDir n d fs of
        (dir,n') -> case map makeFile fs of
            fs' -> (i+1, n', dir:acc1, reverse fs' ++ acc2)
    where
        makeFile f = File (basenameP f) i

------------------------------------------------------------------------

-- | Expand a single directory into a maybe a  pair of the dir name and any files
-- Return any extra directories to search in
--
-- Assumes no evil sym links
--
expandDir :: FilePathP -> IO (Maybe (FilePathP, [FilePathP]),  [FilePathP])
expandDir f | seq f False = undefined -- stricitfy
expandDir f = do
    ls_raw <- handle @SomeException (\e -> hPrint stderr e $> [])
                $ packedGetDirectoryContents f
    let ls = map (\s -> P.intercalate (P.singleton '/') [f,s])
                . sort . filter validFiles $! ls_raw
    ls `seq` return ()
    (fs',ds) <- partition ls
    let fs = filter onlyMp3s fs'
        v = if null fs then Nothing else Just (f,fs)
    return (v,ds)
    where
          notEdge    p = p /= dot && p /= dotdot
          validFiles p = notEdge p
          onlyMp3s   p = mp3 == (P.map toLower . P.drop (P.length p -3) $ p)

          mp3        = "mp3"
          dot        = "."
          dotdot     = ".."

--
-- | Given an the next index into the files array, a directory name, and
-- a list of files in that dir, build a Dir and return the next index
-- into the array
--
listToDir :: Int -> FilePathP -> [FilePathP] -> (Dir, Int)
listToDir n d fs =
        let dir = Dir { dname = packedFileNameEndClean d
                      , dsize = len
                      , dlo   = n
                      , dhi   = n + len - 1
                      } in (dir, n')
    where
        len = length fs
        n'  = n + len

-- | break a list of file paths into a pair of subliests corresponding
-- to the paths that point to files and to directories.
partition :: [FilePathP] -> IO ([FilePathP], [FilePathP])
partition xs | seq xs False = undefined -- how to make `partition' strict
partition [] = return ([],[])
partition (a:xs) = do
    (fs,ds) <- partition xs
    x <- doesFileExist a
    if x then do y <- isReadable a
                 return $! if y then (a:fs, ds) else (fs, ds)
         else return (fs, a:ds)

------------------------------------------------------------------------
--
-- And some more Binary instances
--

{-
instance Binary a => Binary (Array Int a) where
    put arr = do
        put (bounds arr)
        mapM_ put (elems arr)
    get     = do
        ((x,y) :: (Int,Int)) <- get
        (els   :: [a])       <- sequence $ take (y+1) $ repeat get
        return $! listArray (x,y) els
-}

instance Binary File where
    put (File nm i) = put nm >> put i
    get = do
        nm <- get
        i  <- get
        return (File nm i)

instance Binary Dir where
    put (Dir nm sz lo hi) = put nm >> put sz >> put lo >> put hi
    get = do
        nm <- get
        sz <- get
        lo <- get
        hi <- get
        return (Dir nm sz lo hi)

instance Binary Mode where
    put = put . fromEnum
    get = toEnum <$> get

-- How we write everything out
instance Binary SerialT where
    put st = do
        put (ser_farr st, ser_darr st)
        put (ser_indx st)
        put (ser_mode st)

    get    = do
        (a,b)<- get
        i    <- get
        m    <- get
        return $ SerialT {
                    ser_farr = a
                   ,ser_darr = b
                   ,ser_indx = i
                   ,ser_mode = m
                 }

-----------------------------------------------------------------------

-- | Wrap up the values we're going to dump to disk
data SerialT = SerialT {
        ser_farr :: FileArray,
        ser_darr :: DirArray,
        ser_indx :: Int,
        ser_mode :: Mode
     }

writeTree :: FilePath -> SerialT -> IO ()
writeTree f s = L.writeFile f (compress (encode s))

readTree :: FilePath -> IO SerialT
readTree f    = do s <- L.readFile f
                   return (decode (decompress s))
