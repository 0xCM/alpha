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
    Based,
    Radix(..),
    based, base2, base10, base16,
    radix, digits,
    
) where
import Alpha.Canonical.Relations
import Alpha.Canonical.Algebra.Common
import Alpha.Canonical.Algebra.Numeric

import qualified Data.Vector as Vector
import qualified Data.List as List
import qualified Data.Set as Set

-- Represents a numeric base
newtype Radix = Radix Natural
    deriving(Eq,Ord,Num, 
        Subtractive, LeftDistributive, RightDistributive,
        Additive, Divisive,Real,Power, Multiplicative, Unital,
        Incrementable,Decrementable,
        Numeric, ToDouble,ToInt,ToInteger,FromDouble,FromInt,FromNatural,ToNatural)

instance Formattable Radix where
    format (Radix n) = format n

instance Show Radix where
    show = string . format
    
-- | Represents an number in a particular base
data Based (b::Nat) i = Based !i

-- Represents a sequence of digits expressed with respect to a
-- particular basis    
newtype Digits b = Digits [Char]    

-- | Constructs an integer with a specified base
based::forall (n::Nat) a. a -> Based n a
based i = Based i

-- | Constructs a base-2 number
base2::a -> Based 2 a
base2 = based @2

-- | Constructs a base-10 integer
base10::a -> Based 10 a
base10 = based @10

-- | Constructs a base-16 integer
base16::a -> Based 16 a
base16 = based @16

radix::forall b. KnownNat b => Radix
radix = Radix $ natg @b

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
        b = format (nat @b) |> parenthetical |> pad

instance forall b. KnownNat b => Show (Digits b ) where
    show = string . format
                
instance forall b i. (KnownNat b, Formattable i) => Formattable (Based b i) where
    format (Based i) = format i <> b where
        b = format (nat @b) |> parenthetical |> pad

instance forall b i. (KnownNat b, Formattable i) => Show(Based b i) where
    show = string . format
    