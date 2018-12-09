module Alpha.Canonical.Algebra.Exponential
(
    Raised(..), 
    Exponential(..), 

    NaturallyPowered(..),
    IntegrallyPowered(..),
    ApproximatelyPowered(..),

    Powered(..),

    exponential,
    

) where
import Alpha.Base
import Alpha.Native
import Alpha.Canonical.Relations
import Alpha.Canonical.Operators
import Alpha.Canonical.Algebra.Polarity

-- | Represents a family of types that support a notion of (potentially) heterogenous 
-- exponentiation
type family Raised a b

-- | Represents a base raised to a power prior to evaluation
newtype Exponential b p = Exponential (b, p)
    deriving (Eq,Ord)

instance (Powered b p) => Computable (Exponential b p) where
    type Computation (Exponential b p) = Raised b p
    
    compute (Exponential (b,p)) = raise b p

instance (Show b, Show p) => Show (Exponential b p) where
    show (Exponential (b,p)) = (show b) <> "^" <> (show p)

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

class NaturallyPowered a where
    pow::(Unsigned p, Integral p) => a -> p -> a

    (^)::(Unsigned p, Integral p) => a -> p -> a
    (^) = pow
    {-# INLINE (^) #-}
    infixr 8 ^
    
class (Fractional a) => IntegrallyPowered a where
    powi::(Integral p) => a -> p -> a

    (^^)::(Integral p) => a -> p -> a
    (^^) = powi
    {-# INLINE (^^) #-}
    infixr 8 ^^
    
class (Floating a) => ApproximatelyPowered a where
    powa::a -> a -> a

    (**)::a -> a -> a
    (**) = powa
    {-# INLINE (**) #-}
    infixr 8 **
    
-- NaturallyPowered
-------------------------------------------------------------------------------
instance NaturallyPowered Natural where 
    pow = pow'
    {-# INLINE pow #-}
instance NaturallyPowered Integer where 
    pow = pow'
    {-# INLINE pow #-}
instance NaturallyPowered Int where 
    pow = pow'
    {-# INLINE pow #-}
instance NaturallyPowered Int8 where 
    pow = pow'
    {-# INLINE pow #-}
instance NaturallyPowered Int16 where 
    pow = pow'
    {-# INLINE pow #-}
instance NaturallyPowered Int32 where 
    pow = pow'
    {-# INLINE pow #-}
instance NaturallyPowered Int64 where 
    pow = pow'
    {-# INLINE pow #-}
instance NaturallyPowered Word where 
    pow = pow'
    {-# INLINE pow #-}
instance  NaturallyPowered Word8 where 
    pow = pow'
    {-# INLINE pow #-}
instance NaturallyPowered Word16 where 
    pow = pow'
    {-# INLINE pow #-}
instance NaturallyPowered Word32 where 
    pow = pow'
    {-# INLINE pow #-}
instance NaturallyPowered Word64 where 
    pow = pow'
    {-# INLINE pow #-}
instance (Integral n) => NaturallyPowered (Ratio n) where 
    pow = pow'
    {-# INLINE pow #-}    
instance NaturallyPowered Float where 
    pow = pow'
    {-# INLINE pow #-}
instance NaturallyPowered Double where 
    pow = pow'
    {-# INLINE pow #-}
instance NaturallyPowered CFloat where 
    pow = pow'
    {-# INLINE pow #-}
instance NaturallyPowered CDouble where 
    pow = pow'
    {-# INLINE pow #-}

-- ApproximatelyPowered
-------------------------------------------------------------------------------
instance ApproximatelyPowered Float where 
    powa = powa'
    {-# INLINE powa #-}
instance ApproximatelyPowered Double where 
    powa = powa'
    {-# INLINE powa #-}
instance ApproximatelyPowered CFloat where 
    powa = powa'
    {-# INLINE powa #-}
instance ApproximatelyPowered CDouble where 
    powa = powa'
    {-# INLINE powa #-}

-- IntegrallyPowered
-------------------------------------------------------------------------------
instance (Integral n) => IntegrallyPowered (Ratio n) where 
    powi = pow''
    {-# INLINE powi #-}
instance IntegrallyPowered Float where 
    powi = pow''
    {-# INLINE powi #-}
instance IntegrallyPowered Double where 
    powi = pow''
    {-# INLINE powi #-}
instance IntegrallyPowered CFloat where 
    powi = pow''
    {-# INLINE powi #-}
instance IntegrallyPowered CDouble where 
    powi = pow''
    {-# INLINE powi #-}
