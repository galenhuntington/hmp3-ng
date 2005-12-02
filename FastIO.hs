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

import Data.Char                (ord)
import Data.Word                (Word8)
import qualified Data.FastPackedString as P

import Foreign.C.Error
import Foreign.C.String         (CString)
import Foreign.C.Types          (CFile, CInt, CLong, CSize)
import Foreign.ForeignPtr       (withForeignPtr, mallocForeignPtrArray)
import Foreign.Marshal          (peekArray, advancePtr, allocaBytes, alloca)
import Foreign.Marshal.Utils    (with)
import Foreign.Ptr              (Ptr, nullPtr, minusPtr, plusPtr, castPtr)
import Foreign.Storable         (poke, peek)

import System.Directory         (Permissions(..))
import System.IO.Error          (modifyIOError, ioeSetFileName)
import System.IO.Unsafe         (unsafePerformIO)
import System.Posix.Internals
import System.Posix.Types       (Fd, CMode)

import Control.Monad            (liftM)
import Control.Exception        (catch, bracket)


------------------------------------------------------------------------

-- | Packed string version of basename
basenameP :: P.FastString -> P.FastString
basenameP fps = case P.elemIndexLast '/' fps of
    Nothing -> fps
    Just i  -> P.drop (i+1) fps
{-# INLINE basenameP #-}

dirnameP :: P.FastString -> P.FastString
dirnameP fps = case P.elemIndexLast '/' fps of
    Nothing -> P.pack "."
    Just i  -> P.take i fps
{-# INLINE dirnameP #-}

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
--
-- A faster append . append
--
joinPathP :: P.FastString -> P.FastString -> P.FastString
joinPathP f g =
    let (ffp, s, l) = P.toForeignPtr f 
        (fgp, t, m) = P.toForeignPtr g
    in unsafePerformIO $ 
        P.generate len $ \ptr ->
            withForeignPtr ffp $ \fp ->
                withForeignPtr fgp $ \gp -> do
                    c_memcpy ptr (fp `plusPtr` s) l
                    poke   (ptr `plusPtr` l) (sep :: Word8)
                    c_memcpy (ptr `plusPtr` (l + 1)) (gp `plusPtr` t) m
                    return len
    where
      len = P.length f + P.length g + 1
      sep = fromIntegral . ord $ '/'
{-# INLINE joinPathP #-}

-- ---------------------------------------------------------------------

-- readIntPS

-- | readIntPS skips any whitespace at the beginning of its argument, and
-- reads an Int from the beginning of the PackedString.  If there is no
-- integer at the beginning of the string, it returns Nothing, otherwise it
-- just returns the int read, along with a PackedString containing the
-- remainder of its input.  The actual parsing is done by the standard C
-- library function strtol.

readIntPS :: P.FastString -> Maybe (Int, P.FastString)
readIntPS (P.PS x s l) = unsafePerformIO $ withForeignPtr x $ \p-> 
    with p $ \endpp -> do 
       val     <- c_strtol (p `plusPtr` s) endpp 0
       skipped <- (`minusPtr` (p `plusPtr` s)) `liftM` peek endpp
       return $ if skipped == 0
          then Nothing
          else Just (fromIntegral val, P.PS x (s+skipped) (l-skipped))

-- ---------------------------------------------------------------------

-- replicateP w c = P.unfoldr w (\u -> Just (u,u)) c
replicatePS :: Int -> Char -> P.FastString
replicatePS w c = unsafePerformIO $ P.generate w $ \ptr -> go ptr w
    where 
        x = fromIntegral . ord $ c
        go _   0 = return w
        go ptr n = poke ptr x >> go (ptr `plusPtr` 1) (n-1)

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

------------------------------------------------------------------------ 

-- 
-- A wrapper over printf for use in UI.PTimes
-- 
printfPS :: P.FastString -> Int -> Int -> P.FastString
printfPS fmt arg1 arg2 =
    unsafePerformIO $ P.generate lim $ \ptr ->
        P.unsafeUseAsCString fmt $ \c_fmt -> do
            sz' <- c_printf2d ptr (fromIntegral lim) (castPtr c_fmt)
                        (fromIntegral arg1) (fromIntegral arg2)
            return (min lim (fromIntegral sz')) -- snprintf might truncate
    where
      lim = 10 -- NB

foreign import ccall unsafe "static stdio.h snprintf" 
    c_printf2d :: Ptr Word8 -> CSize -> Ptr Word8 -> CInt -> CInt -> IO CInt

