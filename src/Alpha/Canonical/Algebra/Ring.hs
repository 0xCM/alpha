module Alpha.Canonical.Algebra.Ring
(
    Ring(..),
    

) where
import Alpha.Base
import Alpha.Canonical.Algebra.Unital
import Alpha.Canonical.Algebra.Group
import Alpha.Canonical.Algebra.Multiplicative

-- | A ring (with identity)
-- See https://en.wikipedia.org/wiki/Ring_(mathematics)     
class (AbelianGroup a, Multiplicative a, Unital a) 
    => Ring a where

instance Ring Integer    
instance Ring Int
instance Ring Int8
instance Ring Int16
instance Ring Int32
instance Ring Int64
instance (Integral a) => Ring (Ratio a)
        