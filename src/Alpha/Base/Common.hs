{-# LANGUAGE NoStarIsType #-}
-----------------------------------------------------------------------------
-- | Base Data.* modules
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE MagicHash #-}

module Alpha.Base.Common
(
    FiniteBits(..),
    ByteString,    
    Hashable(..),
    Either(..),
    Maybe(..), isNothing, fromJust,
    OrderedEnum(..),
    Enum, fromEnum, toEnum,
    ($),undefined, id, const,    
    error,            
    Bounded, minBound, maxBound,    
    Ptr, Storable, poke, peek, sizeOf, alignment, castPtr,     

    seq,
        
)
where

import Data.Bits(Bits(..),FiniteBits(..))
import Data.Either(Either(..))
import Data.Maybe(Maybe(..),isNothing,fromJust)
import Data.Function(const)
import Data.ByteString(ByteString)
import Data.Ord(Ord)
import Data.Hashable(Hashable(..))
import GHC.Base(($),undefined, id, (.),seq)
import GHC.Enum(Enum, fromEnum,toEnum, Bounded,minBound,maxBound)
import Foreign.Storable(Storable,poke, peek, sizeOf, alignment)
import Foreign.Ptr (Ptr, castPtr)
import Prelude(error)

-- Synonym for combined Ord and Enum constraints
type OrderedEnum a = (Enum a, Ord a)    


