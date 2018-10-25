-----------------------------------------------------------------------------
-- | Fundamental constructs
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Alpha.Canonical 
(
    module Alpha.Algebra,
    module Alpha.Functors,
    module Alpha.Classes,
    Lazy(..),
    Eager(..),
    ifelse,
    NatPair(..)    
)
where


import Data.Text(Text)
import Data.String(String)
import Data.Int(Int)
import GHC.TypeLits(KnownNat,Nat(..))
import Data.Proxy
import Data.Maybe
import Data.Either
import Data.Ord
import Data.Word
import Data.Bool
import Data.Kind(Type)
import Data.Vector(Vector)
import GHC.Num(Num)
import GHC.Real(Integral)
import Alpha.Algebra
import Alpha.Functors
import Alpha.Classes


-- | If the first input value is true, returns the 2nd input value,
-- otherwise, returns the third input value
ifelse::Bool -> a -> a -> a
ifelse x aye no = case x of
            True -> aye
            _ -> no

-- | Constructs a left-valued 'Either'
left :: l -> Either l r
left x = Left x

-- | Constructs a right-valued 'Either'
right :: r -> Either l r
right x = Right x

-- / Constrains a pair of types to be known naturals 
type NatPair n1 n2 = (KnownNat n1, KnownNat n2)

        
map::(Functor f) => (a -> b) -> f a -> f b
map = fmap

empty::(Monoid m) => m
empty = mempty

type family Lazy a 

type family Eager a

type family Boxed a

type family Raw a


