-- Copyright (c) 2004-2008 Don Stewart - http://www.cse.unsw.edu.au/~dons
-- Copyright (c) 2019-2026 Galen Huntington
-- SPDX-License-Identifier: GPL-2.0-or-later

--
-- | The top level application state, and operations on that value.
--
module State where

import Base

import Decoder                  (Status, Frame, Id3, Cmd, cmdToBS, mp3Tool)
import Playlist                 (FileArray, DirArray)
import Style                    (StringA(Fast), UIStyle(warnings))

import Data.ByteString          (hPut)
import GHC.Records
import System.Clock             (TimeSpec(..))
import System.IO                (hFlush)
import System.Process           (ProcessHandle, waitForProcess)
import System.Random            (StdGen)


-- | Player state
data HState = HState
    -- These never change
    { music           :: !FileArray
    , folders         :: !DirArray
    , bootTime        :: !TimeSpec
    , configPath      :: !(Maybe FilePath)     -- style.conf override (CLI)
    , histSize        :: Int
    -- These can
    , current         :: !Int                  -- currently playing mp3
    , cursor          :: !Int                  -- mp3 under the cursor
    , clock           :: !(Maybe Frame)        -- current clock value
    , randomGen       :: !StdGen               -- random seed
    , spawns          :: !Integer              -- count of decoder spawns
    , id3             :: !(Maybe Id3)          -- maybe mp3 id3 info
    , info            :: !(Maybe ByteString)   -- mp3 info
    , status          :: !Status
    , minibuffer      :: !StringA              -- contents of minibuffer
    , modal           :: !(Maybe Modal)        -- modal visible
    , miniFocused     :: !Bool                 -- is the mini buffer focused?
    , mode            :: !Mode
    , uptime          :: !ByteString
    , searchFw        :: !Bool                 -- active search direction
    , searchHist      :: ![ByteString]
    , playHist        :: !(Seq (TimeSpec, Int))
    , uiStyle         :: !UIStyle
    }

instance HasField "size" HState Int where getField hs = length hs.music

data Mode = Once | Loop | Random | Single
    deriving stock (Eq, Bounded, Enum, Show, Read)

-- Each is (timestamp-string, (song-index, song-name)).
type HistDisplay = [(ByteString, (Int, ByteString))]

-- (list-of-keys, description)
type KeysHelp = ([Char], ByteString)

data Modal = HelpModal ![KeysHelp] | ExitModal | HistModal !HistDisplay

-- | A global variable holding the state.
hState :: MVar HState
hState = unsafePerformIO newEmptyMVar
{-# NOINLINE hState #-}

-- | The refresh thread waits on this
modified :: MVar ()
modified = unsafePerformIO newEmptyMVar
{-# NOINLINE modified #-}

-- | Queues a refresh.
setModified :: IO ()
setModified = void $ tryPutMVar modified ()

------------------------------------------------------------------------
-- The decoder.

-- | Decoder read handle (mpg123 stderr).
mpgRead :: MVar Handle
mpgRead = unsafePerformIO newEmptyMVar
{-# NOINLINE mpgRead #-}

data Mpg = Mpg { mpgPH :: !ProcessHandle, writeHM :: !(MVar Handle) }

-- | Decoder process and write handles.
mpgRef :: IORef (Maybe Mpg)
mpgRef = unsafePerformIO $ newIORef Nothing
{-# NOINLINE mpgRef #-}

overseeMpg :: (Handle, Handle, Handle, ProcessHandle) -> IO ()
overseeMpg (writeH, _, errH, mpgPH) = do
    putMVar mpgRead errH
    writeHM <- newMVar writeH
    writeIORef mpgRef $ Just Mpg { mpgPH, writeHM }
    void $ try @SomeException $ waitForProcess mpgPH
    writeIORef mpgRef Nothing
    void $ takeMVar mpgRead

-- | Returns whether succeeded.
sendMpg' :: Cmd -> IO Bool
sendMpg' c = do
    mpg <- readIORef mpgRef
    case mpg of
        Just Mpg { writeHM } -> do
            h <- readMVar writeHM
            fmap isRight $ try @SomeException $
                hPut h (cmdToBS c) *> hPut h "\n" *> hFlush h
        _ -> pure False

-- | Runs above and posts warning on failure.
sendMpg :: Cmd -> IO ()
sendMpg c = do
    ok <- sendMpg' c
    when (not ok) do
        modifyHS_ \st -> st { minibuffer =
            Fast (mp3Tool <> " process not running") st.uiStyle.warnings }

------------------------------------------------------------------------
-- State accessor functions.

-- | Access a component of the state with a projection function
getsHS :: (HState -> a) -> IO a
getsHS f = f <$> readMVar hState

-- | Modify the state with a pure function and no refresh
silentlyModifyHS :: (HState -> HState) -> IO ()
silentlyModifyHS  f = modifyMVar_ hState (pure . f)

modifyHS_ :: (HState -> HState) -> IO ()
modifyHS_ f = silentlyModifyHS f <* setModified

-- | Modify the state returning a value
modifyHS :: (HState -> (HState, a)) -> IO a
modifyHS f = modifyMVar hState (pure . f) <* setModified

