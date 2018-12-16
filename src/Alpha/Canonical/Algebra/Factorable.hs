module Alpha.Canonical.Algebra.Factorable
(
    Factorable(..), Factored(..)
    

) where
import Alpha.Base

type family Factored a = r | r -> a

class Factorable (a::Type) where
    --- | Factors a supplied value into constituent parts
    factor::a -> Factored a

    unfactor::Factored a -> a

