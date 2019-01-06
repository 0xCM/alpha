{-# LANGUAGE NoStarIsType #-}
-----------------------------------------------------------------------------
-- | Base Data.* modules
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE MagicHash #-}

module Alpha.Base.Common
(
    Bits,
    Constraint(..),
    FiniteBits(..),
    ByteString,    
    Hashable(..),
    Either(..),
    Maybe(..), isNothing, fromJust, just, none, isNone, isSome,
    Enum, fromEnum, toEnum,
    ($),undefined, const,    
    error, fst, snd,
    Bounded, minBound, maxBound,    
    Ptr, Storable, poke, peek, sizeOf, alignment, castPtr,     
    when,
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
import GHC.Natural(Natural(..))
import GHC.Base(($),undefined, id, (.),seq,when)
import GHC.Enum(Enum, fromEnum,toEnum, Bounded,minBound,maxBound)
import GHC.Real(Integral(..))
import GHC.Exts(Constraint(..))
import Foreign.Storable(Storable,poke, peek, sizeOf, alignment)
import Foreign.Ptr (Ptr, castPtr)
import Prelude(error,fst,snd)
import Data.Int
import Data.Word
import Data.Bool
import Data.Type.Equality as Equality
import Data.Proxy as Proxy
import qualified GHC.Natural as TN
import qualified GHC.TypeLits as TL


    
-- | Constructs a valued 'Maybe'
just :: a -> Maybe a
just x = Just x

-- | Constructs a non-valued 'Maybe'
none :: Maybe a
none = Nothing

-- | Determines whether the 'Maybe' is non-valued
isNone :: Maybe b -> Bool
isNone = isNothing

-- | Determines whether the 'Maybe' is non-valued
isSome :: Maybe b -> Bool
isSome = not . isNothing



    

