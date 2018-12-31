-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE OverloadedLists #-}
module Alpha.Canonical.Algebra.Based
(
    BasedInt,
    Base(..),
    based, base2, base10, base16,
    base, digits,
    
) where
import Alpha.Canonical.Relations
import Alpha.Canonical.Algebra.IntDomain
import Alpha.Canonical.Algebra.Common

import qualified Data.Vector as Vector
import qualified Data.List as List
import qualified Data.Set as Set

-- Represents a numeric base
newtype Base b = Base Natural
    deriving(Show,Eq,Ord,Num)

-- | Represents an integer with a particular base
data BasedInt (b::Nat) i = BasedInt !i

-- Represents a sequence of digits expressed with respect to a
-- particular basis    
newtype Digits b = Digits [Char]    

-- | Constructs an integer with a specified base
based::forall (n::Nat) i. (IntegralDomain i) => i -> BasedInt n i
based i = BasedInt i

-- | Constructs a base-2 integer
base2::Integer -> BasedInt 2 Integer
base2 = based @2

-- | Constructs a base-10 integer
base10::Integer -> BasedInt 10 Integer
base10 = based @10

-- | Constructs a base-16 integer
base16::Integer -> BasedInt 16 Integer
base16 = based @16

base::forall b. KnownNat b => Base b
base = Base $ natg @b

--encoding64::Vector.Vector Char
--See https://en.wikipedia.org/wiki/Base64 for table    
encoding::Vector.Vector Char
encoding = 
    ['0','1','2','3','4','5','6','7','8','9',
     'A','B','C','D','E','F','G','H','I','J',
     'K','L','M','N','O','P','Q','R','S','T',
     'U','V','W','X','Y','Z']

encode::(Integral i) => i -> Char
encode i = encoding Vector.! (fromIntegral i)

digits::forall b i. (KnownNat b, Integral i) => i -> Digits b
digits i = Digits $ recurse (quotRem' i b) []  
    where
        b = natg @b
        recurse (n,d) r = seq c $
            case n of
            0 -> r'
            _ -> recurse (quotRem' n b) r' 
            where
                c  = encode d
                r' = c : r   

instance forall b. KnownNat b => Formattable (Digits b ) where
    format (Digits n) = format n <> b where 
        b = format (nat @b) |> parenthetical |> spaced

instance forall b. KnownNat b => Show (Digits b ) where
    show = string . format
                
instance forall b i. (KnownNat b, Formattable i) => Formattable (BasedInt b i) where
    format (BasedInt i) = format i <> b where
        b = format (nat @b) |> parenthetical |> spaced

instance forall b i. (KnownNat b, Formattable i) => Show(BasedInt b i) where
    show = string . format
    