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

import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.C.String
import Foreign.C.Error
import Foreign.Ptr

import System.IO.Error
import System.IO

import Control.Monad
import Control.Exception

import System.Posix.Internals

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
    (allocaBytes (P.length path) $ \s -> do
       copyFP path s
       throwErrnoIfNullRetry desc (c_opendir s))
    (\p -> throwErrnoIfMinus1_ desc (c_closedir p))
    (\p -> loop ptr_dEnt p)
  where
    desc = "Utils.packedGetDirectoryContents"

    -- | Copy entry out of directory
    -- Directory entries can be finalised
    make :: CString -> IO P.FastString
    make cstr = do 
        let len = fromIntegral $ c_strlen cstr
        fp <- mallocForeignPtrArray (len+1)
        withForeignPtr fp $ \p -> do
            c_memcpy p (castPtr cstr) len
            poke (p `plusPtr` len) (0 :: Word8)
        return $ P.PS fp 0 len

    loop :: Ptr (Ptr CDirent) -> Ptr CDir -> IO [P.FastString]
    loop ptr_dEnt dir = do
      resetErrno
      r <- readdir dir ptr_dEnt
      if (r == 0)
        then do dEnt <- peek ptr_dEnt
                if (dEnt == nullPtr)
                    then return []
                    else do
                        entry   <- (d_name dEnt >>= make)
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

doesDirectoryExist :: P.FastString -> IO Bool
doesDirectoryExist name = Control.Exception.catch
   (packedWithFileStatus "Utils.doesDirectoryExist" name $ \st -> isDirectory st)
   (\ _ -> return False)

packedWithFileStatus :: String -> P.FastString -> (Ptr CStat -> IO a) -> IO a
packedWithFileStatus loc name f = do
  modifyIOError (`ioeSetFileName` []) $
    allocaBytes sizeof_stat $ \p -> do
      allocaBytes (P.length name) $ \s -> do
        copyFP name s
        throwErrnoIfMinus1Retry_ loc (c_stat s p)
        f p

-- | Destructively copy a packedstring into a cstring
copyFP :: P.FastString -> CString -> IO () -- copy onto stack
copyFP srcfp destp = do
    let (fp, _, _) = P.toForeignPtr srcfp
    withForeignPtr fp $ \ptr -> do
        c_memcpy (castPtr destp) ptr (P.length srcfp)
        poke (destp `plusPtr` P.length srcfp) (0 :: Word8)

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
--
-- Expand directory arguments into their contents
-- Recursive descent
--
-- Do something about memory usage here
--
expandDirectories :: [P.FastString] -> IO [P.FastString]
expandDirectories []     = return []
expandDirectories (f:fs) = do
    ls  <- expandDirectory f
    lss <- expandDirectories fs
    return $! ls ++ lss  -- hmm. append sucks

expandDirectory :: P.FastString -> IO [P.FastString]
expandDirectory f = do
    b  <- doesFileExist f
    if b then return [f]    -- bad
         else do 
    ls_raw <- Control.Exception.handle (\e -> hPutStrLn stderr (show e) >> return []) $ 
                packedGetDirectoryContents f
    let ls = map joinPath . sort . filter notEdge $ ls_raw
    ls `seq` return ()
    (fs,ds) <- partition ls
    ds'     <- expandDirectories ds
    return $! fs ++ ds'
    where
          joinPath g = f `P.append` sep `P.append` g
          notEdge p  = p /= dot && p /= dotdot
          sep        = P.packAddress "/"#
          dot        = P.packAddress "."#
          dotdot     = P.packAddress ".."#

partition :: [P.FastString] -> IO ([P.FastString], [P.FastString]) 
partition [] = return ([],[]) 
partition (a:xs) = do
    (fs,ds) <- partition xs
    b       <- doesFileExist a
    return $! if b then (a:fs, ds) else (fs, a:ds)

-- ---------------------------------------------------------------------

foreign import ccall unsafe "static string.h strlen" c_strlen
    :: CString -> CInt

foreign import ccall unsafe "static string.h memcpy" c_memcpy
    :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
