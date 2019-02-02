{-# LANGUAGE NoStarIsType #-}
-----------------------------------------------------------------------------
-- | Base Data.* modules
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Base.System
(
    IO, IOMode(..), hClose, hFileSize, openFile, print, putStr,
    PrimBase(..),PrimMonad(..), PrimState(..),

    IORef, readIORef, writeIORef, atomicModifyIORef', newIORef,
    unsafeDupablePerformIO,
    shredIO, shredPrim, 
    unsafeCoerce,
    readFileText', readFileBytes',
    withBinaryFile, withFile,
    cpuTimePrecision, getCPUTime, 
    UTCTime(..), NominalDiffTime(..), nominalDay, addUTCTime,diffUTCTime,
    POSIXTime(..), getPOSIXTime,posixSecondsToUTCTime,systemToPOSIXTime,
    SystemTime(..),getSystemTime,utcToSystemTime,systemToUTCTime,
    TimeZone(..),TimeOfDay(..),LocalTime(..),ZonedTime(..),    
) where


import Control.Monad.Primitive
import Data.IORef
import Data.Text(Text)
import Data.ByteString(ByteString)
import Data.String(String)
import GHC.Show
import System.IO (IO, IOMode(..), hClose, hFileSize, openFile, print, putStr,withBinaryFile,withFile)
import System.IO.Unsafe
import System.Console.ANSI
import System.CPUTime(cpuTimePrecision, getCPUTime)
import Data.Time.Clock(UTCTime(..), NominalDiffTime(..), nominalDay, addUTCTime,diffUTCTime)
import Data.Time.Clock.POSIX (POSIXTime(..), getPOSIXTime,posixSecondsToUTCTime,utcTimeToPOSIXSeconds,systemToPOSIXTime)
import Data.Time.Clock.System(SystemTime(..),getSystemTime,utcToSystemTime,systemToUTCTime)
import Data.Time.LocalTime(TimeZone(..),TimeOfDay(..),LocalTime(..),ZonedTime(..))
import Unsafe.Coerce(unsafeCoerce)

import qualified Data.Text.IO as TextIO
import qualified Data.ByteString as BS

readFileText'::String -> IO Text
readFileText' = TextIO.readFile

readFileBytes'::String -> IO ByteString
readFileBytes' = BS.readFile

-- | Just say "no" to the monolithic imprisonment of IO
shredIO::IO a -> a
shredIO = unsafeDupablePerformIO 

shredPrim::(PrimBase m) => m a -> a
shredPrim = unsafeInlinePrim