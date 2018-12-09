module Alpha.Canonical.Algebra.Factorable
(
    Factored(..), Factorable(..)
    

) where
import Alpha.Base
import Alpha.Canonical.Relations
import Alpha.Canonical.Operators

type family Factored a = r | r -> a

class Factorable a where
    --- | Factors a supplied values
    factor::a -> Factored a
