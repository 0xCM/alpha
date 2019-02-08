{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE OverloadedLists #-}
module Alpha.Canonical.Structures.Polynomial
(
    Polynomial(..),
    Poly(..),
    poly,

    PolyN(..),
    polyN,


)
where
import Alpha.Canonical.Algebra
import Alpha.Canonical.Structures.VectorSpace

-- | Represents a polynomial over k of degree n - 1 according to
-- p(x) = (v !! 0)x^0 + (v !! 1)x^1 + (v !! 2)x^2 + ... + (v !! (n-1))x^(n-1)
newtype PolyN n k = PolyN (VecN n k) 
    deriving (Eq,Ord,Generic,Functor,Data,Typeable,Applicative,Foldable,Traversable,Monad,Length)
instance Newtype (PolyN n k)

-- | Represents a polynomial over k of some (finite) degree where
-- v !! i gives the coefficient of x^i and i ranges over 0..(n-1) where n is
-- the degree of the represented polynomial
newtype Poly k = Poly (Vector k) 
    deriving (Eq,Ord,Generic,Functor,Data,Typeable,Applicative,Foldable,Traversable,Monad,Length)
instance Newtype (Poly k)

newtype Monomial n k = Monomial (VecN n k)        
    deriving (Eq,Ord,Generic,Functor,Data,Typeable,Applicative,Foldable,Traversable,Monad)
instance Newtype (Monomial n k)    

newtype Multinomial n k = Multinomial (VecN n k)
    deriving (Eq,Ord,Generic,Functor,Data,Typeable,Applicative,Foldable,Traversable,Monad)
instance Newtype (Multinomial n k)        

class Polynomial a where
    -- | Determines the degree of a polynomial
    degree::a -> Natural


polyN::(KnownNat n, Ring k) => VecN n k -> PolyN n k
polyN = PolyN

-- | Constructs a polynomial from a vector
poly::(Ring k) => Vector k -> Poly k
poly = Poly

instance Polynomial (Poly k) where 
    degree p = length p - 1

instance Ring k => Additive (Poly k) where
    v1 + v2 = (unwrap v1) + (unwrap v2) |> wrap

instance Ring k => Nullary (Poly k) where
    zero = [(zero::k)] |> poly  

instance Ring k => Unital (Poly k) where
    one = [(one::k)] |> poly  

instance Ring k => Negatable (Poly k) where
    negate v = negate (unwrap v) |> wrap

instance Ring k => Subtractive (Poly k) where
    v1 - v2 = (unwrap v1) - (unwrap v2) |> wrap    

instance Ring k => AbelianGroup (Poly k)    

instance Polynomial (PolyN n k) where 
    degree p = length p - 1

instance (KnownNat n, Ring k) => Additive (PolyN n k) where
    v1 + v2 = (unwrap v1) + (unwrap v2) |> wrap

instance (KnownNat n, Ring k) => Nullary (PolyN n k) where
    zero = (zero::VecN n k) |> polyN @n 

instance (KnownNat n, Ring k) => Unital (PolyN n k) where
    one = (one::VecN n k) |> polyN @n 

instance (KnownNat n, Ring k) => Negatable (PolyN n k) where
    negate v = negate (unwrap v) |> wrap
            
instance (KnownNat n, Ring k) => Subtractive (PolyN n k) where
    v1 - v2 = (unwrap v1) - (unwrap v2) |> wrap    

instance (KnownNat n, Ring k) => AbelianGroup (PolyN n k)

-- instance PolyField n k => Multiplicative (Polynomial n k) where
--     v1 * v2 = (unwrap v1) * (unwrap v2) |> wrap

-- instance PolyField n k => LeftDistributive (Polynomial n k)
-- instance PolyField n k => RightDistributive (Polynomial n k)

-- instance PolyField n k => Semiring (Polynomial n k)
-- instance PolyField n k => Ring (Polynomial n k)