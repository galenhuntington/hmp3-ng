{-# OPTIONS -fno-warn-orphans #-}
-- 
-- Copyright (c) 2005 Don Stewart - http://www.cse.unsw.edu.au/~dons
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

import FastIO
import Syntax           (Mode(..))
import Binary           (openBinIO_, Binary(put_, get))
import qualified Data.ByteString.Char8 as P (drop,map,length,pack,ByteString,joinWithChar)

import Data.Maybe       (catMaybes)
import Data.Array       (listArray, elems, bounds, Array)
import Data.Char        (toLower)
import Data.List        (sortBy,sort,foldl',groupBy)

import System.IO        (IOMode(..),hPutStrLn,stderr,openFile,hClose)
import System.Directory (Permissions(readable))
import Control.Exception(handle)
import Control.Monad    (liftM)

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

    ms' <- liftM catMaybes $! loop dirs

    let extras = merge . doOrphans $ os
        ms = ms' ++ extras

    let (_,n,dirls,filels) = foldl' make (0,0,[],[]) ms
        dirsArray = listArray (0,length dirls - 1) (reverse dirls)
        fileArray = listArray (0, n-1) (reverse filels)

    dirsArray `seq` fileArray `seq` return $ (dirsArray, fileArray)

-- | Create nodes based on dirname for orphan files on cmdline
doOrphans :: [FilePathP] -> [(FilePathP, [FilePathP])]
doOrphans []     = []
doOrphans (f:xs) = (dirnameP f, [basenameP f]) : doOrphans xs

-- | Merge entries with the same root node into a single node
merge :: [(FilePathP, [FilePathP])] -> [(FilePathP, [FilePathP])]
merge [] = []
merge xs = 
    let xs' = sortBy  (\a b -> fst a `compare` fst b) xs 
        xs''= groupBy (\a b -> fst a == fst b) xs'
    in catMaybes $ map flatten xs''
  where
    flatten :: [(FilePathP,[FilePathP])] -> Maybe (FilePathP, [FilePathP])
    flatten []     = Nothing    -- can't happen
    flatten (x:ys) = let d = fst x in Just (d, snd x ++ concatMap snd ys)
        
-- | fold builder, for generating Dirs and Files
make :: (Int,Int,[Dir],[File]) -> (FilePathP,[FilePathP]) -> (Int,Int,[Dir],[File])
make (i,n,acc1,acc2) (d,fs) = 
    case listToDir n d fs of 
        (dir,n') -> case map makeFile fs of
            fs' -> (i+1, n', dir:acc1, (reverse fs') ++ acc2)
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
    ls_raw <- Control.Exception.handle (\e -> hPutStrLn stderr (show e) >> return []) $ 
                packedGetDirectoryContents f
    let ls = map (P.joinWithChar '/' f) . sort . filter validFiles $! ls_raw
    ls `seq` return ()
    (fs',ds) <- partition ls
    let fs = filter onlyMp3s fs'
        v = if null fs then Nothing else Just (f,fs)
    return (v,ds)
    where
          notEdge    p = p /= dot && p /= dotdot
          validFiles p = notEdge p
          onlyMp3s   p = mp3 == (P.map toLower . P.drop (P.length p -3) $ p) 

          mp3        = P.pack "mp3"
          dot        = P.pack "."
          dotdot     = P.pack ".."

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
                      , dhi   = n + len - 1 } in (dir, n')
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
    if x then do y <- getPermissions a >>= return . readable
                 return $! if y then (a:fs, ds) else (fs, ds)
         else return (fs, a:ds)

------------------------------------------------------------------------

instance Binary a => Binary (Array Int a) where
    put_ bh arr = do
        put_ bh (bounds arr)
        mapM_ (put_ bh) (elems arr)
    get bh      = do
        ((x,y) :: (Int,Int)) <- get bh
        (els   :: [a])       <- sequence $ take (y+1) $ repeat (get bh)
        return $! listArray (x,y) els

instance Binary File where
    put_ bh (File nm i) = do
        put_ bh nm
        put_ bh i
    get bh = do
        nm <- get bh
        i  <- get bh
        return (File nm i)

instance Binary Dir where
    put_ bh (Dir nm sz lo hi) = do
        put_ bh nm
        put_ bh sz
        put_ bh lo
        put_ bh hi
    get bh = do
        nm <- get bh
        sz <- get bh
        lo <- get bh
        hi <- get bh
        return (Dir nm sz lo hi)

instance Binary Mode where
    put_ bh = put_ bh . fromEnum
    get  bh = liftM toEnum $ get bh

-- How we write everything out
instance Binary SerialT where
    put_ bh st = do
        put_ bh (ser_farr st, ser_darr st)
        put_ bh (ser_indx st)
        put_ bh (ser_mode st)

    get bh     = do
        (a,b)<- get bh
        i    <- get bh
        m    <- get bh
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

--
-- write the arrays out
--
writeTree :: FilePath -> SerialT -> IO ()
writeTree f st = do
    h    <- openFile   f WriteMode
    bh   <- openBinIO_ h
    put_ bh st
    hClose h

--
-- | Read the arrays from a file
-- Read from binMem?
--
readTree :: FilePath -> IO SerialT
readTree f = do
    h    <- openFile   f ReadMode
    bh   <- openBinIO_ h        -- openBinMem
    st   <- get bh
    hClose h
    return st

