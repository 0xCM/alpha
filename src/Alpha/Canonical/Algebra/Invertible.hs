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

