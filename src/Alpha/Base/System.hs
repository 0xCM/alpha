{-# LANGUAGE NoStarIsType #-}
-----------------------------------------------------------------------------
-- | Base Data.* modules
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Base.System
(
    IO, IOMode(..), hClose, hFileSize, openFile, print, putStr,
    PrimBase(..),PrimMonad(..),PrimState(..),
    IORef, readIORef, writeIORef, atomicModifyIORef', newIORef,
    unsafeDupablePerformIO,
    shredIO, shredPrim, 
    unsafeCoerce,
    readFileText', readFileBytes',
) where


import System.IO.Unsafe
import System.Console.ANSI
import Control.Monad.Primitive
import Data.IORef
import GHC.Show
import Data.Text(Text)
import Data.ByteString(ByteString)
import Data.String(String)
import qualified Prelude as P
import System.IO (IO, IOMode(..), hClose, hFileSize, openFile, print, putStr)
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

