-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Base.Array
(
    module X,
    StorableArray, withStorableArray,touchStorableArray,
    LIUArray, listArray,
    SMUSTArray, unsafeNewArraySTUArray_,freezeSTUArray,thawSTUArray, 
    LMBIOArray, unsafeThawIOArray, thawIOArray, freezeIOArray, unsafeFreezeIOArray,newIOArray, unsafeReadIOArray, unsafeWriteIOArray,
    LMBSTArray, newLMBSTArray, writeLMBSTArray, readLMBSTArray, unsafeWriteLMBSTArray, unsafeReadLMBSTArray, 
             unsafeFreezeSTArray, unsafeThawSTArray, freezeSTArray, thawSTArray,
    IArray, array, unsafeAt,
    MArray, marray, freezearray, thawarray, unsafeFreeze,  unsafeThaw, readArray, writeArray,
    UArray, uarray,
    
    
    arraybounds, arrayindices, arrayelem, arrayelems, arrayassocs,    

    testArray1, testArray2, testArray3,  
    

) where
import Control.Monad
import Control.Monad.ST
import Data.Array(Array,listArray)
import Data.Array.Base(IArray, (!), bounds, indices, elems, numElements, assocs, array, freeze, unsafeFreeze, thaw, unsafeThaw, unsafeAt,)
import Data.Array.Base(MArray, MArray(newArray),newListArray, readArray,writeArray,unsafeWrite,unsafeRead)
import Data.Array.Base(unsafeThawIOArray, thawIOArray, freezeIOArray, unsafeFreezeIOArray)
import Data.Array.Base(STUArray, unsafeNewArraySTUArray_,freezeSTUArray,thawSTUArray)
import Data.Array.Storable(StorableArray,withStorableArray,touchStorableArray)
import Data.Array.Unboxed(UArray)
import Data.Ix(Ix(..))
import GHC.Arr(STArray, newSTArray, unsafeWriteSTArray, unsafeReadSTArray, unsafeFreezeSTArray, unsafeThawSTArray,readSTArray, writeSTArray, freezeSTArray, thawSTArray)
import GHC.Arr(amap, accum, accumArray, unsafeAccumArray, unsafeAccum)
import GHC.IOArray(IOArray(..), newIOArray, unsafeReadIOArray, unsafeWriteIOArray)
import Data.Int(Int)
import System.IO(IO(..))
import GHC.Num(Num(..))

import qualified Data.Array.Unboxed as UA
import qualified Data.Array.IArray as IA
import qualified Data.Array.MArray as MA
import qualified Data.Array.Unsafe as USA


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


-- | Constructs a mutable array
marray::(MArray a e m, Ix i) => (i,i) -> e -> m (a i e)
marray = newArray

marray'::(MArray a e m) => [e] -> m (a Int e)
marray' src = newListArray (0, upperix) src where
    upperix = (List.length src) - 1

-- | Constructs an unboxed array
uarray::(Ix i, IArray UArray a) => (i,i) -> [(i,a)] -> UArray i a
uarray = array

arraybounds::(IArray a e, Ix i) => a i e -> (i, i)
arraybounds = bounds

arrayindices::(IArray a e, Ix i) => a i e -> [i]
arrayindices = indices

arrayelem::(IArray a e, Ix i) => a i e -> i -> e
arrayelem = (!)

arrayelems::(IArray a e, Ix i) => a i e -> [e]
arrayelems = elems

arrayassocs::(IArray a e, Ix i) => a i e -> [(i, e)]
arrayassocs = assocs

arraylen::(IArray a e, Ix i) => a i e -> Int
arraylen = numElements

freezearray::(Ix i, MArray a e m, IArray b e) => a i e -> m (b i e)
freezearray = freeze

thawarray::(Ix i, IArray a e, MArray b e m) => a i e -> m (b i e)
thawarray = thaw

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

testArray1::UArray Int Int
testArray1 = array (1,2) [(1,2),(2,1)]

testArray2::UArray Int Int
testArray2 = uarray (1,2) [(1,2),(2,1)]
    

testArray3 = do 
    arr <- marray' [0..9] ::IO (StorableArray Int Int)
    a <- readArray arr 1
    withStorableArray arr 
        (\ptr -> memset ptr 0 40)
    b <- readArray arr 1
    return (a,b)
    
testArray4 = do 
    arr <- marray' [0..9] ::IO (LMBIOArray Int Int)
    a <- readArray arr 1
    writeArray arr 1 64
    b <- readArray arr 1 
    return (a,b)

testArray5 = do 
    arr <- marray' [0..9] ::ST s (LMBSTArray s Int Int)
    a <- readArray arr 1
    writeArray arr 1 64
    b <- readArray arr 1
    return (a,b)
    

foreign import ccall unsafe "string.h" 
    memset::Ptr a -> CInt -> CSize -> IO ()