module Alpha.Canonical.Algebra.Factorable
(
    Factored(..), Factorable(..)
    

) where
import Alpha.Base

type family Factored a = r | r -> a

class Factorable a where
    --- | Factors a supplied values
    factor::a -> Factored a
