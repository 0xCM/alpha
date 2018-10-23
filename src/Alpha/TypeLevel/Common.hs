{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Alpha.TypeLevel.Common
(
    Witness(..),
    Value(..)

)
where

import Data.Proxy
import Data.Type.Equality as Equality
import Data.Functor.Const
import Data.Hashable
import Data.Kind
import Data.Maybe(isJust)
import Data.Int
import Data.Bool
import Data.Maybe
import Data.Eq
import Data.Ord
import Data.String
import GHC.TypeLits
import GHC.Num
import GHC.Show
import Prelude(undefined)

type family Value k :: Type    

class Witness (w :: k) where
    value :: Value k

type instance Value Bool = Bool
type instance Value Symbol = String
type instance Value Nat = Integer
    
instance KnownSymbol s => Witness (s :: Symbol) where
    value = symbolVal @s undefined
    
instance KnownNat n => Witness (n :: Nat) where
    value = natVal @n undefined    

-- type a :*: b = (a, b)
-- data a :|: b = OrL a | OrR b
    
------------------------------------------------------------------------
-- CoercibleF

-- | An instance of 'CoercibleF' gives a way to coerce between
--   all the types of a family.  We generally use this to witness
--   the fact that the type parameter to @rtp@ is a phantom type
--   by giving an implementation in terms of Data.Coerce.coerce.
class CoercibleF (rtp :: k -> *) where
    coerceF :: rtp a -> rtp b
  
instance CoercibleF (Const x) where
    coerceF (Const x) = Const x
  
------------------------------------------------------------------------
-- EqF

-- | @EqF@ provides a method @eqF@ for testing whether two parameterized
-- types are equal.
--
-- Unlike 'TestEquality', this only works when the type arguments are
-- the same, and does not provide a proof that the types have the same
-- type when they are equal. Thus this can be implemented over
-- parameterized types that are unable to provide evidence that their
-- type arguments are equal.
class EqF (f :: k -> *) where
    eqF :: f a -> f a -> Bool
  
instance Eq a => EqF (Const a) where
    eqF (Const x) (Const y) = x == y
  
------------------------------------------------------------------------
-- PolyEq

-- | A polymorphic equality operator that generalizes 'TestEquality'.
class PolyEq u v where
    polyEqF :: u -> v -> Maybe (u :~: v)
  
    polyEq :: u -> v -> Bool
    polyEq x y = isJust (polyEqF x y)
  
------------------------------------------------------------------------
-- HashableF

-- | A default salt used in the implementation of 'hash'.
defaultSalt :: Int
#if WORD_SIZE_IN_BITS == 64
defaultSalt = 0xdc36d1615b7400a4
#else
defaultSalt = 0x087fc72c
#endif
{-# INLINE defaultSalt #-}

-- | A parameterized type that is hashable on all instances.
class HashableF (f :: k -> *) where
  hashWithSaltF :: Int -> f tp -> Int

  -- | Hash with default salt.
  hashF :: f tp -> Int
  hashF = hashWithSaltF defaultSalt

instance Hashable a => HashableF (Const a) where
  hashWithSaltF s (Const x) = hashWithSalt s x


