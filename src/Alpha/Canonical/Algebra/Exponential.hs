module Alpha.Canonical.Algebra.Exponential
(
    Exponential(..), 
    Powered(..),
    exponential,
    Raised(..),
    

) where
import Alpha.Base
import Alpha.Native



-- | Represents a family of types that support a notion of (potentially) heterogenous 
-- exponentiation
type family Raised a b

-- | Represents a base raised to a power prior to evaluation
newtype Exponential b p = Exponential (b, p)
    deriving (Eq,Ord)

-- | Constructs, but does not evaluate, an exponential representation
exponential::b -> p -> Exponential b p
exponential b p = Exponential (b,p)

class Powered b p where
    -- | Exponentiates b to the power of p
    raise::b -> p -> Raised b p

    -- | Infix synonym for 'raise'
    (>^<)::b -> p -> Raised b p
    (>^<) = raise
    infixr 8 >^<
