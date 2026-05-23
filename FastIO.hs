-- Copyright (c) 2005-2008 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (c) 2019-2021, 2025 Galen Huntington
-- SPDX-License-Identifier: GPL-2.0-or-later

-- | ByteString versions of some common IO functions

module FastIO where

import Base

import qualified Data.ByteString.Char8 as B

import System.Posix.Files.ByteString
import System.Posix.Directory.ByteString

------------------------------------------------------------------------

--  Use every nth frame.  1 for no dropping.
dropRate :: Int
dropRate = 4   -- used to be 10, but computers are faster

-- | Packed string version of basename
basenameP :: ByteString -> ByteString
basenameP fps = case B.elemIndexEnd '/' fps of
    Nothing -> fps
    Just i  -> B.drop (i+1) fps
{-# INLINE basenameP #-}

dirnameP :: ByteString -> ByteString
dirnameP fps = case B.elemIndexEnd '/' fps of
    Nothing -> "."
    Just i  -> B.take i fps
{-# INLINE dirnameP #-}

-- | Packed version of listDirectory
packedGetDirectoryContents :: ByteString -> IO [ByteString]
packedGetDirectoryContents fp = bracket (openDirStream fp) closeDirStream
    $ \ds -> fmap (filter (\p -> p/="." && p/=".."))
        $ sequenceWhile (not . B.null) $ repeat $ readDirStream ds

doesFileExist :: ByteString -> IO Bool
doesFileExist fp = catch @SomeException
   (not . isDirectory <$> getFileStatus fp)
   (\_ -> pure False)

doesDirectoryExist :: ByteString -> IO Bool
doesDirectoryExist fp = catch @SomeException
   (isDirectory <$> getFileStatus fp)
   (\_ -> pure False)

packedFileNameEndClean :: ByteString -> ByteString
packedFileNameEndClean name =
  case B.unsnoc name of
    Just (name', ec) | ec == '\\' || ec == '/'
         -> packedFileNameEndClean name'
    _    -> name

-- ---------------------------------------------------------------------

data FiltHandle = FiltHandle { filtHandle :: !Handle, frameCount :: !(IORef Int) }

newFiltHandle :: Handle -> IO FiltHandle
newFiltHandle h = FiltHandle h <$> newIORef 0

-- | Read a line from a file stream connected to an external prcoess,
-- Returning a ByteString.
getPacket :: FiltHandle -> IO ByteString
getPacket (FiltHandle fp _) = B.hGetLine fp

-- | Check if it's one of every dropRate packets.
-- We don't need to process all since there are so many.
checkF :: FiltHandle -> IO Bool
checkF (FiltHandle _ ir) = do
  modifyIORef' ir (\x -> (x+1) `mod` dropRate)
  i <- readIORef ir
  pure $ dropRate==1 || i==1

-- ---------------------------------------------------------------------

isReadable :: ByteString -> IO Bool
isReadable fp = fileAccess fp True False False

------------------------------------------------------------------------ 

-- ---------------------------------------------------------------------
trim :: ByteString -> ByteString
{-# INLINE trim #-}
trim = B.dropWhileEnd isSpace . B.dropSpace

