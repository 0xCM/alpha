-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Base.Foreign
(
    module X,
    Ptr, 
    Storable, poke, peek, sizeOf, alignment, castPtr, 
    CShort(..),CUShort(..), CInt(..), CUInt(..), CLong(..), CULong(..), CLLong(..), 
    CULLong(..), CDouble(..), CFloat(..), CSize(..),CSigAtomic(..),CBool(..),
    CIntPtr(..), CUIntPtr(..),
    CChar(..), CSChar(..),CUChar(..), CWchar(..),
    CUSeconds(..), CSUSeconds(..), CClock(..),CTime(..), 
)
where
import Alpha.Base.System as X
import Alpha.Base.Collections as X
import Foreign.Storable(Storable,poke, peek, sizeOf, alignment)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.C

