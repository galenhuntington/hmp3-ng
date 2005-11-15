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

-- miscellaneous utilites

module Utils where

import Data.Char
import Data.List
import Data.Word
import qualified Data.FastPackedString as P

import Text.Printf

import Foreign.Marshal
import Foreign.Storable
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.C.String
import Foreign.C.Error
import Foreign.Ptr

import System.IO.Error
import System.Time
import System.IO

import Control.Monad
import Control.Exception

import System.Posix.Internals

------------------------------------------------------------------------

-- some filename manipulation stuff

--
-- | </>, <.> : join two path components
--
infixr 6 </>
infixr 6 <.>

(</>), (<.>), (<>) :: FilePath -> FilePath -> FilePath
[] </> b = b
a  </> b = a ++ "/" ++ b

[] <.> b = b
a  <.> b = a ++ "." ++ b

[] <> b = b
a  <> b = a ++ b

basename :: FilePath -> FilePath
basename p = reverse $ takeWhile (/= '/') $ reverse p

-- | Packed string version of basename
basenameP :: P.FastString -> P.FastString
basenameP fps = case P.elemIndexLast '/' fps of
    Nothing -> fps
    Just i  -> P.drop (i+1) fps

dirname :: FilePath -> FilePath
dirname p  =
    case reverse $ dropWhile (/= '/') $ reverse p of
        [] -> "."
        p' -> p'

dropSuffix :: FilePath -> FilePath
dropSuffix f = reverse . tail . dropWhile (/= '.') $ reverse f

joinPath :: FilePath -> FilePath -> FilePath
joinPath p q =
    case reverse p of
      '/':_ -> p ++ q
      []    -> q
      _     -> p ++ "/" ++ q

------------------------------------------------------------------------

-- | 'dropSpace' takes as input a String and strips spaces from the
--   prefix as well as the postfix of the String. Example:
--
-- > dropSpace "   abc  " ===> "abc"
dropSpace :: [Char] -> [Char]
dropSpace = let f = reverse . dropWhile isSpace in f . f

-- | Drop suffix and prefix quotes from a `shown' string
--
clean :: String -> String
clean = let f = reverse . dropWhile (== '\"') in f . f 

-- | Split a list into pieces that were held together by glue.  Example:
--
-- > split ", " "one, two, three" ===> ["one","two","three"]
split :: Eq a => [a] -- ^ Glue that holds pieces together
      -> [a]         -- ^ List to break into pieces
      -> [[a]]       -- ^ Result: list of pieces
split glue xs = split' xs
    where
    split' [] = []
    split' xs' = piece : split' (dropGlue rest)
        where (piece, rest) = breakOnGlue glue xs'
    dropGlue = drop (length glue)


-- | Break off the first piece of a list held together by glue,
--   leaving the glue attached to the remainder of the list.  Example:
--   Like break, but works with a [a] match.
--
-- > breakOnGlue ", " "one, two, three" ===> ("one", ", two, three")
breakOnGlue :: (Eq a) => [a] -- ^ Glue that holds pieces together
            -> [a]           -- ^ List from which to break off a piece
            -> ([a],[a])     -- ^ Result: (first piece, glue ++ rest of list)
breakOnGlue _ [] = ([],[])
breakOnGlue glue rest@(x:xs)
    | glue `isPrefixOf` rest = ([], rest)
    | otherwise = (x:piece, rest')
        where (piece, rest') = breakOnGlue glue xs
{-# INLINE breakOnGlue #-}

------------------------------------------------------------------------

drawUptime :: ClockTime -> ClockTime -> P.FastString
drawUptime before now =
    let r = diffClockTimes now before
        s = tdSec  r
        h = quot s (60 * 60)
        m = quot s 60
    in P.pack $! ((printf "%3d:%02d" (h::Int) (m::Int)) :: String)
        
------------------------------------------------------------------------
-- | Repeat an action
repeatM_ :: forall m a. Monad m => m a -> m ()
repeatM_ a = a >> repeatM_ a
{-# SPECIALIZE repeatM_ :: IO a -> IO () #-}
{-# INLINE repeatM_ #-}

------------------------------------------------------------------------

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
    (P.unsafeUseAsCString path $ \s -> do -- might have been shortened. poke!
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
doesFileExist name =
    Control.Exception.catch
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
      P.unsafeUseAsCString (name) $ \s -> do
        throwErrnoIfMinus1Retry_ loc (c_stat s p)
        f p

-- hmm. if the FastString is ever shortened, then we lose the null
-- terminator property, and can't use it with CStrings.
packedFileNameEndClean :: P.FastString -> P.FastString
packedFileNameEndClean = id
{-
  if i > 0 && (ec == '\\' || ec == '/') then
     packedFileNameEndClean (P.take i name)
   else
     name
  where
      i  = (P.length name) - 1
      ec = name `P.index` i
-}

isDirectory :: Ptr CStat -> IO Bool
isDirectory stat = do
  mode <- st_mode stat
  return (s_isdir mode)

foreign import ccall unsafe "static string.h strlen" c_strlen
    :: CString -> CInt

foreign import ccall unsafe "static string.h memcpy" c_memcpy
    :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
