-- 
-- Copyright (c) 2005-2008 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (c) 2019 Galen Huntington
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

import Syntax                   (Pretty(ppr))

import qualified Data.ByteString.Char8 as P
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B

import Data.Word                (Word8)
import qualified Data.ByteString.UTF8 as UTF8
import Foreign.C.Error
import Foreign.C.Types          (CInt(..), CSize(..))
import Foreign.Marshal          (allocaBytes)
import Foreign.Ptr              (Ptr, castPtr, plusPtr)
import Foreign.Storable         (peekElemOff)
import Foreign.ForeignPtr

import qualified System.Directory as Dir
import System.IO.Error          (modifyIOError, ioeSetFileName)
import System.IO.Unsafe         (unsafePerformIO)
import System.IO                (Handle,hFlush)
import Data.IORef
import System.Posix.Internals

import Control.Exception        (catch, SomeException)

------------------------------------------------------------------------

--  Copied from C comment:
-- * Note that mpg321 (only) provides --skip-printing-frames=N
-- * I guess we could have used that.
dropRate :: Int
dropRate = 6   -- used to be 10, but computers are faster

-- | Packed string version of basename
basenameP :: P.ByteString -> P.ByteString
basenameP fps = case P.elemIndexEnd '/' fps of
    Nothing -> fps
    Just i  -> P.drop (i+1) fps
{-# INLINE basenameP #-}

dirnameP :: P.ByteString -> P.ByteString
dirnameP fps = case P.elemIndexEnd '/' fps of
    Nothing -> "."
    Just i  -> P.take i fps
{-# INLINE dirnameP #-}

--
-- | Packed version of get directory contents
-- Have them just return CStrings, then pack lazily?
--
packedGetDirectoryContents :: P.ByteString -> IO [P.ByteString]
packedGetDirectoryContents = do
  fmap (map UTF8.fromString) . Dir.listDirectory . UTF8.toString

-- packed version:
doesFileExist :: P.ByteString -> IO Bool
doesFileExist name = Control.Exception.catch
   (packedWithFileStatus "Utils.doesFileExist" name $ \st -> do
        b <- isDirectory st; return (not b))
   (\ (_ :: SomeException) -> return False)

-- packed version:
doesDirectoryExist :: P.ByteString -> IO Bool
doesDirectoryExist name = Control.Exception.catch
   (packedWithFileStatus "Utils.doesDirectoryExist" name $ \st -> isDirectory st)
   (\ (_ :: SomeException) -> return False)

packedWithFileStatus :: String -> P.ByteString -> (Ptr CStat -> IO a) -> IO a
packedWithFileStatus loc name f = do
  modifyIOError (`ioeSetFileName` []) $
    allocaBytes sizeof_stat $ \p -> do
      B.useAsCString name $ \s -> do    -- i.e. every string is duplicated
        throwErrnoIfMinus1Retry_ loc (c_stat s p)
        f p

packedFileNameEndClean :: P.ByteString -> P.ByteString
packedFileNameEndClean name =
  if i > 0 && (ec == '\\' || ec == '/') then
     packedFileNameEndClean (P.take i name)
   else
     name
  where
      i  = (P.length name) - 1
      ec = name `P.index` i

isDirectory :: Ptr CStat -> IO Bool
isDirectory stat = do
  mode <- st_mode stat
  return (s_isdir mode)

-- ---------------------------------------------------------------------

data FiltHandle = FiltHandle { filtHandle :: !Handle, frameCount :: !(IORef Int) }

-- | Read a line from a file stream connected to an external prcoess,
-- Returning a ByteString.
getPacket :: FiltHandle -> IO P.ByteString
getPacket (FiltHandle fp _) = B.hGetLine fp

-- | Check if it's one of every dropRate packets.
-- We don't need to process all since there are so many.
checkF :: FiltHandle -> IO Bool
checkF (FiltHandle _ ir) = do
  modifyIORef' ir (\x -> (x+1) `mod` dropRate)
  i <- readIORef ir
  return $ (i==1)

-- ---------------------------------------------------------------------

getPermissions :: P.ByteString -> IO Dir.Permissions
getPermissions = Dir.getPermissions . UTF8.toString

-- ---------------------------------------------------------------------
-- | Send a msg over the channel to the decoder
send :: Pretty a => Handle -> a -> IO ()
send h m = P.hPut h (ppr m) >> P.hPut h "\n" >> hFlush h

------------------------------------------------------------------------ 

-- | 'dropSpaceEnd' efficiently returns the 'ByteString' argument with
-- white space removed from the end. I.e.
-- 
-- > reverse . (dropWhile isSpace) . reverse == dropSpaceEnd
--
-- but it is more efficient than using multiple reverses.
--
dropSpaceEnd :: P.ByteString -> P.ByteString
{-# INLINE dropSpaceEnd #-}
dropSpaceEnd (B.PS x s l) = unsafePerformIO $ withForeignPtr x $ \p -> do
    i <- lastnonspace (p `plusPtr` s) (l-1)
    return $! if i == (-1) then B.empty else B.PS x s (i+1)
    where
        lastnonspace :: Ptr Word8 -> Int -> IO Int
        lastnonspace ptr n
            | ptr `seq` n `seq` False = undefined
            | n < 0     = return n
            | otherwise = do w <- peekElemOff ptr n
                             if B.isSpaceWord8 w then lastnonspace ptr (n-1)
                                                 else return n

------------------------------------------------------------------------ 

-- 
-- A wrapper over printf for use in UI.PTimes
-- 
printfPS :: P.ByteString -> Int -> Int -> P.ByteString
printfPS fmt arg1 arg2 =
    unsafePerformIO $ B.createAndTrim lim $ \ptr ->
        B.useAsCString fmt $ \c_fmt -> do
            sz' <- c_printf2d ptr (fromIntegral lim) (castPtr c_fmt)
                        (fromIntegral arg1) (fromIntegral arg2)
            return (min lim (fromIntegral sz')) -- snprintf might truncate
    where
      lim = 10 -- NB

-- ---------------------------------------------------------------------

foreign import ccall unsafe "static stdio.h snprintf" 
    c_printf2d :: Ptr Word8 -> CSize -> Ptr Word8 -> CInt -> CInt -> IO CInt
