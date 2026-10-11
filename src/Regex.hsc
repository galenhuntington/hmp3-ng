module Regex where

import Base

import Data.ByteString qualified as P
import Data.ByteString.Unsafe qualified as P
import Data.Int -- needed for whatever regoff_t might be
import Foreign.C.Types (CInt(..), CSize(..))
import Foreign.C.String (CString)
import Foreign.Concurrent (newForeignPtr)
import Foreign.ForeignPtr hiding (newForeignPtr)
import Foreign.Marshal.Alloc
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (pokeByteOff)

#include <regex.h>

newtype Regex = Regex (ForeignPtr ())

compOptions :: CInt
compOptions = #{const REG_EXTENDED} + #{const REG_NOSUB} + #{const REG_ICASE}

compile :: ByteString -> Maybe Regex
compile pat = unsafePerformIO $
    P.useAsCString pat $ \patC -> do
        preg <- mallocBytes #{size regex_t}
        rc <- regcomp preg patC compOptions
        if rc == 0
            then Just . Regex <$> newForeignPtr preg (regfree preg *> free preg)
            else Nothing <$ free preg

execPosix, execSE, exec :: Regex -> ByteString -> Bool

execPosix (Regex fp) str = unsafePerformIO $
    withForeignPtr fp $ \preg ->
    P.useAsCString str $ \cstr ->
        (== 0) <$> regexec preg cstr 0 nullPtr 0

-- More efficient.
execSE (Regex fp) str = unsafePerformIO $
    withForeignPtr fp $ \preg ->
    P.unsafeUseAsCStringLen str $ \(cstr, len) ->
    allocaBytes #{size regmatch_t} $ \pmatch -> do
        #{poke regmatch_t, rm_so} pmatch (0 :: #{type regoff_t})
        #{poke regmatch_t, rm_eo} pmatch (fromIntegral len :: #{type regoff_t})
        (== 0) <$> regexec preg cstr 0 pmatch #{const REG_STARTEND}

exec =
#ifdef REG_STARTEND
    execSE
#else
    execPosix
#endif


foreign import ccall unsafe "regcomp"
    regcomp :: Ptr () -> CString -> CInt -> IO CInt

foreign import ccall unsafe "regexec"
    regexec :: Ptr () -> CString -> CSize -> Ptr () -> CInt -> IO CInt

foreign import ccall unsafe "regfree"
    regfree :: Ptr () -> IO ()

