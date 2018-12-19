module Alpha.Canonical.Algebra.Normed
(    
    Normed(..)
) where
import Alpha.Base
import Alpha.Native

class Normed a b where
    norm::a -> b

    



