-- 
-- Copyright (c) 2005-2008 Don Stewart - http://www.cse.unsw.edu.au/~dons
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

-- | ByteString versions of some common IO functions

module FastIO where

import Base

import Syntax                   (Pretty(ppr))

import qualified Data.ByteString.Char8 as P
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B

import System.Posix.Files.ByteString
import System.Posix.Directory.ByteString

import System.IO                (hFlush)
import Control.Monad.Extra      (sequenceWhile)

------------------------------------------------------------------------

--  Use every nth frame.  1 for no dropping.
dropRate :: Int
dropRate = 4   -- used to be 10, but computers are faster

-- | Packed string version of basename
basenameP :: ByteString -> ByteString
basenameP fps = case P.elemIndexEnd '/' fps of
    Nothing -> fps
    Just i  -> P.drop (i+1) fps
{-# INLINE basenameP #-}

dirnameP :: ByteString -> ByteString
dirnameP fps = case P.elemIndexEnd '/' fps of
    Nothing -> "."
    Just i  -> P.take i fps
{-# INLINE dirnameP #-}

-- | Packed version of listDirectory
packedGetDirectoryContents :: ByteString -> IO [ByteString]
packedGetDirectoryContents fp = bracket (openDirStream fp) closeDirStream
    $ \ds -> fmap (filter (\p -> p/="." && p/=".."))
        $ sequenceWhile (not . P.null) $ repeat $ readDirStream ds

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
  case P.unsnoc name of
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

-- ---------------------------------------------------------------------
-- | Send a msg over the channel to the decoder
send :: Pretty a => Handle -> a -> IO ()
send h m = P.hPut h (ppr m) >> P.hPut h "\n" >> hFlush h

------------------------------------------------------------------------ 

-- | 'dropSpaceEnd' efficiently returns the 'ByteString' argument with
-- white space removed from the end. I.e.,
-- 
-- > reverse . (dropWhile isSpace) . reverse == dropSpaceEnd
dropSpaceEnd :: ByteString -> ByteString
{-# INLINE dropSpaceEnd #-}
dropSpaceEnd bs = P.take (P.length bs - count) bs where
    count = B.foldl' go 0 bs
    go n c = if B.isSpaceWord8 c then n+1 else 0

