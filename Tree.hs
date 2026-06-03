-- Copyright (c) 2005-8 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (c) 2019-2020, 2025-2026 Galen Huntington
-- SPDX-License-Identifier: GPL-2.0-or-later

--
-- functions for manipulating file trees
--

module Tree where

import Base hiding (partition)

import qualified Data.ByteString.Char8 as P
import qualified Data.Map.Strict as M

import Data.Array
import System.IO        (hPrint, stderr)
import System.Posix.FilePath         (RawFilePath, takeFileName, takeDirectory
                                     , dropTrailingPathSeparator, takeExtension)
import System.Posix.Files.ByteString     (getFileStatus, isDirectory, fileAccess)
import System.Posix.Directory.ByteString (openDirStream, readDirStream, closeDirStream)


type FilePathP = RawFilePath

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

data Tree = Tree !DirArray !FileArray

--
-- | Given the start directories, populate the dirs and files arrays
--
buildTree :: [FilePathP] -> IO Tree
buildTree fs = do
    (os, dirs) <- partition fs    -- note we will lose the ordering of files given on cmd line.

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
doOrphans :: [FilePathP] -> [(FilePathP, [FilePathP])]
doOrphans = map \f -> (takeDirectory f, [takeFileName f])

-- | Merge entries with the same root node into a single node
merge :: [(FilePathP, [FilePathP])] -> [(FilePathP, [FilePathP])]
merge = M.assocs . M.fromListWith (flip (++))

-- | fold builder, for generating Dirs and Files
make :: (Int,Int,[Dir],[File]) -> (FilePathP,[FilePathP]) -> (Int,Int,[Dir],[File])
make (i,n,acc1,acc2) (d,fs) =
    let (dir, n') = listToDir n d fs
        fs'= map makeFile fs
    in (i+1, n', dir:acc1, reverse fs' ++ acc2)
  where
    makeFile f = File (takeFileName f) i

------------------------------------------------------------------------

-- | Expand a single directory into a maybe a  pair of the dir name and any files
-- Return any extra directories to search in
--
-- Assumes no evil sym links
--
expandDir :: FilePathP -> IO (Maybe (FilePathP, [FilePathP]),  [FilePathP])
expandDir !f = do
    ls_raw <- handle @SomeException (\e -> hPrint stderr e $> [])
                $ getDirContents f
    let ls = (map \s -> P.intercalate (P.singleton '/') [f,s])
                . sort . filter validFiles $ ls_raw
    (fs',ds) <- partition ls
    let fs = filter onlyMp3s fs'
        v = if null fs then Nothing else Just (f,fs)
    pure (v,ds)
  where
    validFiles = not . P.isPrefixOf "."
    onlyMp3s   = (== ".mp3") . P.map toLower . takeExtension

--
-- | Given an the next index into the files array, a directory name, and
-- a list of files in that dir, build a Dir and return the next index
-- into the array
--
listToDir :: Int -> FilePathP -> [FilePathP] -> (Dir, Int)
listToDir n d fs =
        let dir = Dir { dname = dropTrailingPathSeparator d
                      , dsize = len
                      , dlo   = n
                      , dhi   = n + len - 1
                      } in (dir, n')
    where
        len = length fs
        n'  = n + len

-- | break a list of file paths into a pair of sublists corresponding
-- to the paths that point to files and to directories.
partition :: [FilePathP] -> IO ([FilePathP], [FilePathP])
partition [] = pure ([],[])
partition (a:xs) = do
    (fs,ds) <- partition xs
    x <- isFile a
    if x then do y <- isReadable a
                 pure if y then (a:fs, ds) else (fs, ds)
         else pure (fs, a:ds)

-- | A directory's entries (excluding "." and ".."), as raw paths.
getDirContents :: RawFilePath -> IO [RawFilePath]
getDirContents fp = bracket (openDirStream fp) closeDirStream
    $ \ds -> fmap (filter (\p -> p /= "." && p /= ".."))
        $ sequenceWhile (not . P.null) $ repeat $ readDirStream ds

-- | Does the path name an existing non-directory?  (False on any error.)
isFile :: RawFilePath -> IO Bool
isFile fp = catch @SomeException
   (not . isDirectory <$> getFileStatus fp)
   (\_ -> pure False)

isReadable :: RawFilePath -> IO Bool
isReadable fp = fileAccess fp True False False

