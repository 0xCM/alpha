-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Base.Array
(
    module X,
    StorableArray,withStorableArray,touchStorableArray,
    LIUArray, arrayAssocs, listArray,
    SMUSTArray, unsafeNewArraySTUArray_,freezeSTUArray,thawSTUArray, 
    LMBIOArray, unsafeThawIOArray, thawIOArray, freezeIOArray, unsafeFreezeIOArray,newIOArray, unsafeReadIOArray, unsafeWriteIOArray,
    LMBSTArray, newLMBSTArray, writeLMBSTArray, readLMBSTArray, unsafeWriteLMBSTArray, unsafeReadLMBSTArray, 
             unsafeFreezeSTArray, unsafeThawSTArray, freezeSTArray, thawSTArray,
    IArray, array, unsafeAt,
    MArray, marray, freeze, unsafeFreeze, thaw, unsafeThaw, readArray,writeArray,
    storableExample,             

) where
import Control.Monad
import Control.Monad.ST
import Data.Array.Base(IArray,  array, freeze, unsafeFreeze, thaw, unsafeThaw, unsafeAt,)
import Data.Array.Base(MArray, MArray(newArray),newListArray, readArray,writeArray,unsafeWrite,unsafeRead)
import Data.Array.Base(unsafeThawIOArray, thawIOArray, freezeIOArray, unsafeFreezeIOArray)
import Data.Array.Base(STUArray, unsafeNewArraySTUArray_,freezeSTUArray,thawSTUArray)
import Data.Array.Storable(StorableArray,withStorableArray,touchStorableArray)
import GHC.Arr(Array,listArray)
import GHC.Arr(STArray, newSTArray, unsafeWriteSTArray, unsafeReadSTArray, unsafeFreezeSTArray, unsafeThawSTArray,readSTArray, writeSTArray, freezeSTArray, thawSTArray)
import GHC.Arr(amap, accum, accumArray, unsafeAccumArray, unsafeAccum,assocs)
import GHC.IOArray(IOArray(..), newIOArray, unsafeReadIOArray, unsafeWriteIOArray)
import Data.Int(Int)
import System.IO(IO(..))
import Data.Ix(Ix(..))
import GHC.Num(Num(..))


import Alpha.Base.Foreign as X
import qualified Data.List as List

class Touchable a where
    type Touched a
    type Touched a = ()
    touch::a -> Touched a

-- | A strict, mutable, unboxed array in the ST monad
type SMUSTArray = STUArray

-- | A lazy, immutable unboxed array
type LIUArray = Array

-- | A lazy, mutable boxed array in the state monad
type LMBSTArray = STArray

-- | A lazy, mutable, boxed array in the IO monad
type LMBIOArray = IOArray

arrayAssocs::Ix i => LIUArray i e -> [(i, e)]
arrayAssocs = assocs

-- | Constructs a mutable array
marray'::(MArray a e m, Ix i) => (i,i) -> e -> m (a i e)
marray' = newArray

marray::(MArray a e m) => [e] -> m (a Int e)
marray src = newListArray (0, upperix) src where
    upperix = (List.length src) - 1

writeLMBSTArray::Ix i => LMBSTArray s i e -> i -> e -> ST s ()
writeLMBSTArray = writeSTArray

unsafeWriteLMBSTArray::LMBSTArray s i e -> Int -> e -> ST s ()
unsafeWriteLMBSTArray = unsafeWriteSTArray

unsafeReadLMBSTArray::LMBSTArray s i e -> Int -> ST s e
unsafeReadLMBSTArray = unsafeReadSTArray

readLMBSTArray::Ix i => LMBSTArray s i e -> i -> ST s e
readLMBSTArray = readSTArray

newLMBSTArray::Ix i => (i,i) -> e -> ST s (LMBSTArray s i e)
newLMBSTArray = newSTArray

instance Touchable (StorableArray i e) where
    type Touched (StorableArray i e) = IO()
    touch = touchStorableArray

lmbstExample = do 
    arr <- marray [0..9] ::ST s (LMBSTArray s Int Int)
    a <- readArray arr 1
    writeArray arr 1 64
    b <- readArray arr 1
    return (a,b)

ioExample = do 
    arr <- marray [0..9] ::IO (LMBIOArray Int Int)
    a <- readArray arr 1
    writeArray arr 1 64
    b <- readArray arr 1 
    return (a,b)

storableExample = do 
    arr <- marray [0..9] ::IO (StorableArray Int Int)
    a <- readArray arr 1
    withStorableArray arr 
        (\ptr -> memset ptr 0 40)
    b <- readArray arr 1
    return (a,b)

foreign import ccall unsafe "string.h" 
    memset::Ptr a -> CInt -> CSize -> IO ()