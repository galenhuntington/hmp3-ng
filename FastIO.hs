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

-- FastPackedString versions of some common IO functions

module FastIO where

import Data.Char
import Data.List hiding (partition)
import Data.Word
import qualified Data.FastPackedString as P

import Foreign.C.Error
import Foreign.C.String
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable

import System.IO.Error
import System.Directory
import System.Posix.Types       ( Fd, CMode )
import System.Posix.Internals

import Control.Monad
import Control.Exception

import GHC.IOBase

------------------------------------------------------------------------

-- | Packed string version of basename
basenameP :: P.FastString -> P.FastString
basenameP fps = case P.elemIndexLast '/' fps of
    Nothing -> fps
    Just i  -> P.drop (i+1) fps

--
-- | Packed version of get args.
--
packedGetArgs :: IO [P.FastString]
packedGetArgs = 
  alloca $ \ p_argc ->  
  alloca $ \ p_argv -> do
   getProgArgv p_argc p_argv
   p    <- fromIntegral `liftM` peek p_argc
   argv <- peek p_argv
   peekArray (p - 1) (advancePtr argv 1) >>= return . map P.packCString

foreign import ccall unsafe "RtsAPI.h getProgArgv"
  getProgArgv :: Ptr CInt -> Ptr (Ptr CString) -> IO ()

--
-- | Packed version of get directory contents
-- Have them just return CStrings, then pack lazily?
--
packedGetDirectoryContents :: P.FastString -> IO [P.FastString]
packedGetDirectoryContents path = do
  modifyIOError (`ioeSetFileName` (P.unpack path)) $
   alloca $ \ ptr_dEnt ->
     bracket
    (P.useAsCString path $ \s ->
       throwErrnoIfNullRetry desc (c_opendir s))
    (\p -> throwErrnoIfMinus1_ desc (c_closedir p))
    (\p -> loop ptr_dEnt p)
  where
    desc = "Utils.packedGetDirectoryContents"

    loop :: Ptr (Ptr CDirent) -> Ptr CDir -> IO [P.FastString]
    loop ptr_dEnt dir = do
      resetErrno
      r <- readdir dir ptr_dEnt
      if (r == 0)
        then do dEnt <- peek ptr_dEnt
                if (dEnt == nullPtr)
                    then return []
                    else do  -- copy entry out before we free:
                        entry   <- (d_name dEnt >>= copyCStringToFastString)
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
doesFileExist :: P.FastString -> IO Bool
doesFileExist name = Control.Exception.catch
   (packedWithFileStatus "Utils.doesFileExist" name $ \st -> do 
        b <- isDirectory st; return (not b))
   (\ _ -> return False)

-- packed version:
doesDirectoryExist :: P.FastString -> IO Bool
doesDirectoryExist name = Control.Exception.catch
   (packedWithFileStatus "Utils.doesDirectoryExist" name $ \st -> isDirectory st)
   (\ _ -> return False)

packedWithFileStatus :: String -> P.FastString -> (Ptr CStat -> IO a) -> IO a
packedWithFileStatus loc name f = do
  modifyIOError (`ioeSetFileName` []) $
    allocaBytes sizeof_stat $ \p -> do
      P.useAsCString name $ \s -> do    -- i.e. every string is duplicated
        throwErrnoIfMinus1Retry_ loc (c_stat s p)
        f p

packedFileNameEndClean :: P.FastString -> P.FastString
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

-- | Duplicate a CString as a FastString
copyCStringToFastString :: CString -> IO P.FastString
copyCStringToFastString cstr = do 
    let len = fromIntegral $ c_strlen cstr
    fp <- mallocForeignPtrArray (len+1)
    withForeignPtr fp $ \p -> do
        c_memcpy p (castPtr cstr) len
        poke (p `plusPtr` len) (0 :: Word8)
    return $! P.PS fp 0 len

-- ---------------------------------------------------------------------

-- | Read a line from a file stream connected to an external prcoess,
-- Returning a FastString. Note that the underlying C code is dropping
-- redundant @F frames for us.
getFilteredPacket :: Ptr CFile -> IO P.FastString
getFilteredPacket fp = P.generate size $ \p -> do 
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

getPermissions :: P.FastString -> IO Permissions
getPermissions name = do
  P.useAsCString name $ \s -> do
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

