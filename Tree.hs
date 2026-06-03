-- Copyright (c) 2005-8 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (c) 2019-2020, 2025-2026 Galen Huntington
-- SPDX-License-Identifier: GPL-2.0-or-later

--
-- functions for manipulating file trees
--

module Tree (module Tree, RawFilePath) where

import Base

import qualified Data.ByteString.Char8 as P
import qualified Data.Map.Strict as M

import Data.Array
import System.IO (hPrint, stderr)
import System.Posix.FilePath
import System.Posix.Files.ByteString (getFileStatus, isDirectory, fileAccess)
import System.Posix.Directory.Traversals (getDirectoryContents)


-- | A filesystem hierarchy is flattened to just the end nodes
type DirArray = Array Int Dir

-- | The complete list of .mp3 files
type FileArray = Array Int File

-- | A directory entry is the directory name, and a list of bound
-- indicies into the Files array.
data Dir  =
    Dir { dname :: !RawFilePath        -- ^ directory name
        , dsize :: !Int              -- ^ number of file entries
        , dlo   :: !Int              -- ^ index of first entry
        , dhi   :: !Int }            -- ^ index of last entry

-- Most data is allocated in this structure
data File =
    File { fbase :: !RawFilePath      -- ^ basename of file
         , fdir  :: !Int }          -- ^ index of Dir entry 

data Tree = Tree !DirArray !FileArray

--
-- | Given the start directories, populate the dirs and files arrays
--
buildTree :: [RawFilePath] -> IO Tree
buildTree fs = do
    (os, dirs) <- sift fs    -- note we will lose the ordering of files given on cmd line.

    let loop []     = pure []
        loop (a:xs) = do
            (m, ds) <- expandDir a
            ms      <- loop $ ds ++ xs  -- add to work list
            pure $ m : ms

    ms' <- catMaybes <$> loop dirs

    let extras = merge . doOrphans $ os
        ms = ms' ++ extras

    let (_,n,dirls,filels) = foldl' make (0,0,[],[]) ms
        dirsArray = listArray (0,length dirls - 1) (reverse dirls)
        fileArray = listArray (0, n-1) (reverse filels)

    pure $! Tree dirsArray fileArray

-- | Is the tree empty?
isEmpty :: Tree -> Bool
isEmpty (Tree _ files) = null files

-- | Create nodes based on dirname for orphan files on cmdline
doOrphans :: [RawFilePath] -> [(RawFilePath, [RawFilePath])]
doOrphans = map \f -> (takeDirectory f, [takeFileName f])

-- | Merge entries with the same root node into a single node
merge :: [(RawFilePath, [RawFilePath])] -> [(RawFilePath, [RawFilePath])]
merge = M.assocs . M.fromListWith (flip (++))

-- | fold builder, for generating Dirs and Files
make :: (Int,Int,[Dir],[File]) -> (RawFilePath,[RawFilePath]) -> (Int,Int,[Dir],[File])
make (i,n,acc1,acc2) (d,fs) =
    let (dir, n') = listToDir n d fs
        fs'= map makeFile fs
    in (i+1, n', dir:acc1, reverse fs' ++ acc2)
  where
    makeFile f = File (takeFileName f) i

------------------------------------------------------------------------

-- | Expand a single directory into a maybe a pair of the dir name and any files
-- Return any extra directories to search in
--
-- Assumes no evil sym links
--
expandDir :: RawFilePath -> IO (Maybe (RawFilePath, [RawFilePath]),  [RawFilePath])
expandDir !f = do
    ls_raw <- handle @SomeException (\e -> hPrint stderr e $> [])
        $ map snd <$> getDirectoryContents f
    let ls = map (f </>) $ sort $ filter notHidden ls_raw
    (fs', ds) <- sift ls
    let fs = filter isMp3 fs'
        v = guard (not $ null fs) *> Just (f, fs)
    pure (v, ds)
  where
    notHidden = not . P.isPrefixOf "."
    isMp3     = (== ".mp3") . P.map toLower . takeExtension

-- | Given an index into the files array, a directory name, and
-- a list of files in that dir, build a Dir and return the next index
-- into the array
listToDir :: Int -> RawFilePath -> [RawFilePath] -> (Dir, Int)
listToDir n d fs = (dir, n') where
    dir = Dir
        { dname = dropTrailingPathSeparator d
        , dsize = len
        , dlo   = n
        , dhi   = n + len - 1
        }
    len = length fs
    n'  = n + len

-- | Break a pair of sublists of files and directories, filtering
-- out ones without permission.
sift :: [RawFilePath] -> IO ([RawFilePath], [RawFilePath])
sift []     = pure ([], [])
sift (p:ps) = do
    it@(fs,ds) <- sift ps
    st <- getFileStatus p
    let isDir = isDirectory st
    perm <- fileAccess p True False isDir
    pure if
        | not perm -> it
        | isDir    -> (fs, p:ds)
        | True     -> (p:fs, ds)

