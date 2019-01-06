{-# LANGUAGE DataKinds #-}
module Alpha.Canonical.Structures.Polynomial
(
    Polynomial(..),

    polyN
)
where
import Alpha.Canonical.Algebra
import Alpha.Canonical.Structures.VectorSpace

-- | Captures the invariant that a vector of lenth n + 1 is required to
-- represent a polynomial of degree n
type PolyField n m k = (KnownNat n, KnownNat m, m ~ (n + 1), Field k)


newtype Polynomial n m k = Polynomial (VecN m k) 
    deriving (Eq,Ord,Generic,Functor,Data,Typeable,Applicative,Foldable,Traversable,Monad,IsList)

instance Newtype (Polynomial n m k)

newtype Monomial n m k = Monomial (VecN m k)        
    deriving(Generic)

instance Newtype (Monomial n m k)    

newtype Multinomial n m k = Multinomial (VecN m k)
    deriving(Generic) 
instance Newtype (Multinomial n m k)        

instance PolyField n m k => Additive (Polynomial n m k) where
    v1 + v2 = (unwrap v1) + (unwrap v2) |> wrap

instance PolyField n m k => Nullary (Polynomial n m k) where
    zero = (zero::VecN m k) |> polyN @n 

instance PolyField n m k => Unital (Polynomial n m k) where
    one = (one::VecN m k) |> polyN @n 

instance PolyField n m k => Negatable (Polynomial n m k) where
    negate v = negate (unwrap v) |> wrap
            
instance PolyField n m k => Subtractive (Polynomial n m k) where
    v1 - v2 = (unwrap v1) - (unwrap v2) |> wrap

instance PolyField n m k => Multiplicative (Polynomial n m k) where
    v1 * v2 = (unwrap v1) * (unwrap v2) |> wrap

instance PolyField n m k => AbelianGroup (Polynomial n m k)
        

polyN::PolyField n m k => VecN m k -> Polynomial n m k
polyN = Polynomial