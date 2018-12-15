module Alpha.Canonical.Algebra.Ring
(
    Ring(..),
    LeftIdeal(..), RightIdeal(..), Ideal(..)    

) where
import Alpha.Base
import Alpha.Canonical.Element
import Alpha.Canonical.Algebra.Multiplicative
import Alpha.Canonical.Algebra.Additive
import Alpha.Canonical.Algebra.Nullary
import Alpha.Canonical.Algebra.Negatable
import Alpha.Canonical.Algebra.Invertible
import Alpha.Canonical.Algebra.Unital
import Alpha.Canonical.Algebra.Scaled
import Alpha.Canonical.Relations

-- | A ring (with identity)
-- See https://en.wikipedia.org/wiki/Ring_(mathematics)     
class (Additive a, Nullary a, Unital a, Multiplicative a, Negatable a)
    => Ring a where

--class (Ring a, LeftScalar a a) => LeftIdeal a where

-- | Represents a left ideal of a ring r
data RI r = RI

-- | Represents a right ideal of a ring r
data IR r = IR

type LeftIdeal r = (Ring r, Subset (RI r) r, LeftScalar r (RI r))

type RightIdeal r = (Ring r, Subset (IR r) r, RightScalar (IR r) r)

type Ideal r = (LeftIdeal r, RightIdeal r)    


instance Ring Integer    
instance Ring Int
instance Ring Int8
instance Ring Int16
instance Ring Int32
instance Ring Int64
instance (Integral a) => Ring (Ratio a)
        