module Alpha.Canonical.Algebra.Invertible
(    
    Invertible(..),

) where
import Alpha.Base
import Alpha.Native

-- | Characterizes types whose values are closed under 
-- mulitiplicative inversion 
class Invertible a where
    invert::a -> a

instance Integral a => Invertible (Ratio a) where
    invert 0 = 0
    invert x = recip x