-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module Alpha.Canonical.Common.Partnership
(
    Partner(..),
    Partnership(..)
)
where

import Alpha.Base as X

type family Partner (i::Nat) a 
type instance Partner 0 (a,b) = a
type instance Partner 1 (a,b) = b

type instance Partner 0 (a,b,c) = a
type instance Partner 1 (a,b,c) = b
type instance Partner 2 (a,b,c) = c

-- | Represents an n-ary association among partnered values    
class KnownNat n => Partnership n a where
    partner::a -> Partner n a

instance Partnership 0 (a,b) where
    partner (a,_) = a
instance Partnership 1 (a,b) where
    partner (_,b) = b

instance Partnership 0 (a,b,c) where
    partner (a,_,_) = a
instance Partnership 1 (a,b,c) where
    partner (_,b,_) = b    
instance Partnership 2 (a,b,c) where
    partner (_,_,c) = c
    