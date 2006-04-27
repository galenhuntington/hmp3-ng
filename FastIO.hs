-- 
-- Copyright (c) 2005-6 Don Stewart - http://www.cse.unsw.edu.au/~dons
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

import Data.Word                (Word8)
import Foreign.C.Error
import Foreign.C.String         (CString)
import Foreign.C.Types          (CFile, CInt, CLong, CSize)
import Foreign.Marshal          (allocaBytes, alloca)
import Foreign.Ptr              (Ptr, nullPtr, castPtr)
import Foreign.Storable         (peek)

import System.Directory         (Permissions(..))
import System.IO.Error          (modifyIOError, ioeSetFileName)
import System.IO.Unsafe         (unsafePerformIO)
import System.IO                (Handle,hFlush)
import System.Posix.Internals
import System.Posix.Types       (Fd, CMode)

import Control.Exception        (catch, bracket)


------------------------------------------------------------------------

-- | Packed string version of basename
basenameP :: P.ByteString -> P.ByteString
basenameP fps = case P.elemIndexLast '/' fps of
    Nothing -> fps
    Just i  -> P.drop (i+1) fps
{-# INLINE basenameP #-}

dirnameP :: P.ByteString -> P.ByteString
dirnameP fps = case P.elemIndexLast '/' fps of
    Nothing -> P.pack "."
    Just i  -> P.take i fps
{-# INLINE dirnameP #-}

--
-- | Packed version of get directory contents
-- Have them just return CStrings, then pack lazily?
--
packedGetDirectoryContents :: P.ByteString -> IO [P.ByteString]
packedGetDirectoryContents path = do
  modifyIOError (`ioeSetFileName` (P.unpack path)) $
   alloca $ \ ptr_dEnt ->
     bracket
    (B.useAsCString path $ \s ->
       throwErrnoIfNullRetry desc (c_opendir s))
    (\p -> throwErrnoIfMinus1_ desc (c_closedir p))
    (\p -> loop ptr_dEnt p)
  where
    desc = "Utils.packedGetDirectoryContents"

    loop :: Ptr (Ptr CDirent) -> Ptr CDir -> IO [P.ByteString]
    loop ptr_dEnt dir = do
      resetErrno
      r <- readdir dir ptr_dEnt
      if (r == 0)
        then do dEnt <- peek ptr_dEnt
                if (dEnt == nullPtr)
                    then return []
                    else do  -- copy entry out before we free:
                        entry   <- B.copyCString `fmap` d_name dEnt 
                        freeDirEnt dEnt
                        entries <- loop ptr_dEnt dir
                        return $! (entry:entries)

        else do errno <- getErrno
                if (errno == eINTR) 
                    then loop ptr_dEnt dir 
                    else do let (Errno eo) = errno
                            if (eo == end_of_dir)
                                then return []
                                else throwErrno desc

-- packed version:
doesFileExist :: P.ByteString -> IO Bool
doesFileExist name = Control.Exception.catch
   (packedWithFileStatus "Utils.doesFileExist" name $ \st -> do 
        b <- isDirectory st; return (not b))
   (\ _ -> return False)

-- packed version:
doesDirectoryExist :: P.ByteString -> IO Bool
doesDirectoryExist name = Control.Exception.catch
   (packedWithFileStatus "Utils.doesDirectoryExist" name $ \st -> isDirectory st)
   (\ _ -> return False)

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

-- | Read a line from a file stream connected to an external prcoess,
-- Returning a ByteString. Note that the underlying C code is dropping
-- redundant \@F frames for us.
getFilteredPacket :: Ptr CFile -> IO P.ByteString
getFilteredPacket fp = B.generate size $ \p -> do 
    i <- c_getline p fp
    if i == -1 
        then throwErrno "FastIO.packedHGetLine"
        else return i
    where
        size = 1024 + 1 -- seems unlikely

-- convert a Haskell-side Fd to a FILE*.
fdToCFile :: Fd -> IO (Ptr CFile)
fdToCFile = c_openfd

-- ---------------------------------------------------------------------

getPermissions :: P.ByteString -> IO Permissions
getPermissions name = do
  B.useAsCString name $ \s -> do
  readp <- c_access s r_OK
  write <- c_access s w_OK
  exec  <- c_access s x_OK
  packedWithFileStatus "FastIO.getPermissions" name $ \st -> do
  is_dir <- isDirectory st
  return (
    Permissions {
      readable   = readp == 0,
      writable   = write == 0,
      executable = not is_dir && exec == 0,
      searchable = is_dir && exec == 0
    }
   )

-- ---------------------------------------------------------------------
-- | Send a msg over the channel to the decoder
send :: Pretty a => Handle -> a -> IO ()
send h m = P.hPut h (ppr m) >> P.hPut h nl >> hFlush h
    where
      nl = P.pack "\n"

------------------------------------------------------------------------ 

-- 
-- A wrapper over printf for use in UI.PTimes
-- 
printfPS :: P.ByteString -> Int -> Int -> P.ByteString
printfPS fmt arg1 arg2 =
    unsafePerformIO $ B.generate lim $ \ptr ->
        B.unsafeUseAsCString fmt $ \c_fmt -> do
            sz' <- c_printf2d ptr (fromIntegral lim) (castPtr c_fmt)
                        (fromIntegral arg1) (fromIntegral arg2)
            return (min lim (fromIntegral sz')) -- snprintf might truncate
    where
      lim = 10 -- NB

-- ---------------------------------------------------------------------

foreign import ccall safe "utils.h forcenext"
    forceNextPacket :: IO ()

foreign import ccall safe "utils.h getline" 
    c_getline :: Ptr Word8 -> Ptr CFile -> IO Int

foreign import ccall safe "utils.h openfd"
    c_openfd  :: Fd -> IO (Ptr CFile)

foreign import ccall unsafe "static string.h strlen" 
    c_strlen  :: CString -> CInt

foreign import ccall unsafe "static string.h memcpy" 
    c_memcpy  :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()

foreign import ccall unsafe "__hscore_R_OK" r_OK :: CMode
foreign import ccall unsafe "__hscore_W_OK" w_OK :: CMode
foreign import ccall unsafe "__hscore_X_OK" x_OK :: CMode

foreign import ccall unsafe "static stdlib.h strtol" c_strtol
    :: Ptr Word8 -> Ptr (Ptr Word8) -> Int -> IO CLong

foreign import ccall unsafe "static stdio.h snprintf" 
    c_printf2d :: Ptr Word8 -> CSize -> Ptr Word8 -> CInt -> CInt -> IO CInt
