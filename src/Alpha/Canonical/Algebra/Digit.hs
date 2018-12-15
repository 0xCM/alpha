-----------------------------------------------------------------------------
-- | 
-- Copyright   : Original author and Chris Moore(c) 0xCM, 2018
-- License     :  BSD/MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
module Alpha.Canonical.Algebra.Digit
(
    Digital(..), Digit(..), 
)
where
import Alpha.Base
import Alpha.Canonical.Text
import Alpha.Canonical.Operators
--import Alpha.Canonical.Algebra.Numeric

import qualified Data.List as List

-- Represents a base-10 digit, i.e. one of 0 - 9
newtype Digit n = Digit Word8 
    deriving (Eq,Ord)


instance forall n. KnownNat n => Bounded (Digit n ) where
    minBound = Digit 0
    maxBound = Digit 9

instance forall n. KnownNat n => Formattable (Digit n) where
    format (Digit n) = format n

instance forall n. KnownNat n => Show (Digit n) where
    show = string . format
    
class KnownNat n => Digital n where
    digit::Digit n
    digit = Digit (natg @n)
    
instance Digital 0    
instance Digital 1
instance Digital 2
instance Digital 3
instance Digital 4
instance Digital 5
instance Digital 6
instance Digital 7
instance Digital 8
instance Digital 9

-- instance forall b n. (KnownNat b, Integral n) => ToInteger (Digits b n) where
--     integer = fromIntegral . undigits
-- instance forall b n. (KnownNat b, Integral n) => ToNatural (Digits b n) where
--     natural = fromIntegral . undigits    
-- instance forall b n. (KnownNat b, Integral n) => ToWord (Digits b n) where
--     word = fromIntegral . undigits        
-- instance forall b n. (KnownNat b, Integral n) => ToInt (Digits b n) where
--     int = fromIntegral . undigits            