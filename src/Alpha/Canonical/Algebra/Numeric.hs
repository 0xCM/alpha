-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE BangPatterns #-}
module Alpha.Canonical.Algebra.Numeric
(
    module X,
    Numeric(..),
    RealNumber(..), 
    ExtendedReal(..),
    Magnitude(..),
    Rational(..),
    RationalNumber(..),
    UnsignedRational(..),
    ComplexNumber(..),
    ComplexNumeric(..),
    RealInterval(..),
    rational,
    urational,
    real,
    ureal,
    complex,
    euler,
    xreal,
    xrealNI,
    xrealPI,
    closed,
    open,
    lopen,
    ropen

) where
import Alpha.Canonical.Relations as X
import Alpha.Canonical.Algebra.Powers as X
import Alpha.Canonical.Algebra.Successive as X
import Alpha.Canonical.Algebra.Negatable as X
import Alpha.Canonical.Algebra.Action as X

-- | Represents a real number
newtype RealNumber a = RealNumber a
    deriving (Eq, Ord, Generic, Data, Typeable, Num, Real, Fractional, Floating)
    deriving (Additive, Subtractive, Negatable, Nullary, Multiplicative, Unital, Divisive)
    deriving (LeftDistributive,RightDistributive)
    deriving (Incrementable,Decrementable)
    deriving (LT,GT,LTEQ,GTEQ,Comparable)
    deriving (ToDouble,ToInt,ToInteger,FromDouble,FromInt,FromNatural,ToNatural)
    deriving (Power,IntegralPower,FloatingPower,Magnitude)
    deriving (Numeric)

-- | Represents a rational number
newtype RationalNumber a = RationalNumber (Ratio a)
    deriving (Eq, Ord, Generic, Data, Typeable)
    deriving (Enum, Num, Real, Fractional, RealFrac)
    deriving (Additive, Subtractive, Negatable, Nullary, Multiplicative, Unital, Divisive)
    deriving (LeftDistributive,RightDistributive)
    deriving (Incrementable,Decrementable)
    deriving (LT,GT,LTEQ,GTEQ,Comparable)
    deriving (ToDouble,FromDouble,FromInt,FromNatural)
    deriving (Power,IntegralPower,Magnitude)
    deriving (Numeric)

-- | Represents a complex number
newtype ComplexNumber a = ComplexNumber (a,a)
    deriving(Eq, Additive, Subtractive, Negatable, Unital, Nullary)

-- | Represents an unsigned rational number
newtype UnsignedRational a = UnsignedRational (RationalNumber a)
    deriving (Eq, Ord, Generic, Data, Typeable)
    deriving (Enum, Num, Real, Fractional, RealFrac)
    deriving (Additive, Subtractive, Negatable, Nullary, Multiplicative, Unital, Divisive)
    deriving (LeftDistributive,RightDistributive)
    deriving (Incrementable,Decrementable)
    deriving (LT,GT,LTEQ,GTEQ,Comparable)
    deriving (ToDouble,FromDouble,FromInt,FromNatural)
    deriving (Power,IntegralPower,Magnitude)
    deriving (Numeric,Rational)
    deriving (Show,Formattable) via (RationalNumber a)
instance Newtype (UnsignedRational a)    

-- | Represents an unsigned rational number
newtype UnsignedReal a = UnsignedReal (RealNumber a)
    deriving (Eq, Ord, Generic, Data, Typeable, Num, Real, Fractional, Floating)
    deriving (Additive, Subtractive, Negatable, Nullary, Multiplicative, Unital, Divisive)
    deriving (LeftDistributive,RightDistributive)
    deriving (Incrementable,Decrementable)
    deriving (LT,GT,LTEQ,GTEQ,Comparable)
    deriving (ToDouble,ToInt,ToInteger,FromDouble,FromInt,FromNatural,ToNatural)
    deriving (Power,IntegralPower,FloatingPower,Magnitude)
    deriving (Numeric)
    deriving (Show,Formattable) via (RealNumber a)
instance Newtype (UnsignedReal a)    

-- | Represents an extended real number
-- | See https://en.wikipedia.org/wiki/Extended_real_number_line
data ExtendedReal a 
    = InfiniteReal (Infinity a)
    | BoundedReal (RealNumber a)
    | UndefinedReal (Indeterminate a)
    deriving (Eq, Ord, Generic, Data, Typeable)

-- | Represents an interval of real numbers    
data RealInterval a 
    = ClosedInterval (RealNumber a, RealNumber a)
    -- |^ A closed inverval [a,b] where a and b are finite real numbers
    | OpenInterval (ExtendedReal a, ExtendedReal a)
    -- |^ An open interval (a,b) where a and b are extended real numbers
    | LeftOpenInterval (ExtendedReal a, RealNumber a)
    -- |^ A left-open (a,b] where a is an extended real number and b is a finite real number
    | RightOpenInterval (RealNumber a, ExtendedReal a)
    -- |^ A right-open [a,b) where a is finite real number and b is an extended real

class NumericInterval a where
    
    
closed::RealNumber a -> RealNumber a -> RealInterval a
closed left right = ClosedInterval(left,right)

open::ExtendedReal a -> ExtendedReal a -> RealInterval a
open left right = OpenInterval (left,right)

lopen::ExtendedReal a -> RealNumber a -> RealInterval a
lopen left right = LeftOpenInterval (left,right)

ropen::RealNumber a -> ExtendedReal a -> RealInterval a
ropen left right = RightOpenInterval (left,right)

type instance Individual (RationalNumber a) = a    
type instance Individual (ComplexNumber a) = a
type instance Individual (UnsignedRational a) = a

class Magnitude a where
    abs::a -> a
    default abs::(Nullary a, TotalOrd a, Negatable a) => a -> a
    abs a = ifelse (a >= zero) a (negate a)    

type NumericContext a = (Additive a, Subtractive a, Multiplicative a, Divisive a, Distributive a, Num a, Power a, Incrementable a, Decrementable a)

class NumericContext a => Numeric a where
    num::a -> a
    num = id
    {-# INLINE num #-}

class Rational a where
    numerator:: a -> Individual a
    
    denominator::a -> Individual a

class ComplexNumeric a where
    conjugate::a -> a

    re::a -> Individual a

    im::a -> Individual a

-- Constructs a 'Complex' number from an ordered pair
complex::(a,a) -> ComplexNumber a
complex (r,i) = ComplexNumber (r,i)


-- | Evaluates Euler's formula for e^{ix} := cos x + i(sin x)
-- See https://en.wikipedia.org/wiki/Euler%27s_formula
euler::Trigonometric a => a -> ComplexNumber a
euler x = complex (cos x, sin x)            
        
-- Constructs a 'Rational' number from a 'Real' number
rational::Integral i => i -> i -> RationalNumber i
rational a b = RationalNumber  $ frac' a b
{-# INLINE rational #-} 

-- | Constructs a 'UnsignedRational' value 
urational::Integral i  => i -> i -> UnsignedRational i
urational a b = UnsignedRational $ rational (abs' a) (abs' b)
{-# INLINE urational #-}

-- | Constructs a 'RealNumber' value 
real::Num a => a -> RealNumber a
real = RealNumber
{-# INLINE real #-}

-- | Constructs a 'UnsignedReal' value 
ureal::Num a => a -> UnsignedReal a
ureal =  UnsignedReal . real . abs'
{-# INLINE ureal #-}
    
-- | Constructs an extended negative infinite value
xrealNI::ExtendedReal a
xrealNI = InfiniteReal neginf
{-# INLINE xrealNI #-}

-- | Constructs an extended positive infinite value
xrealPI::ExtendedReal a
xrealPI = InfiniteReal posinf
{-# INLINE xrealPI #-}

-- | Constructs a bounded extended real
xreal::RealNumber a -> ExtendedReal a
xreal = BoundedReal 
{-# INLINE xreal #-}


-- * Numeric instances
-------------------------------------------------------------------------------
instance Numeric Natural
instance Numeric Integer
instance Numeric Int
instance Numeric Int8
instance Numeric Int16
instance Numeric Int32
instance Numeric Int64
instance Numeric Word
instance Numeric Word8
instance Numeric Word16
instance Numeric Word32
instance Numeric Word64
instance (Integral a, Ord a) => Numeric (Ratio a)
instance Numeric Float
instance Numeric Double
instance Numeric CFloat
instance Numeric CDouble

-- * Magnitude instances
-------------------------------------------------------------------------------
instance Magnitude Natural
instance Magnitude Integer
instance Magnitude Int
instance Magnitude Int8
instance Magnitude Int16
instance Magnitude Int32
instance Magnitude Int64
instance Magnitude Word
instance Magnitude Word8
instance Magnitude Word16
instance Magnitude Word32
instance Magnitude Word64
instance (Integral a, Ord a) => Magnitude (Ratio a)
instance Magnitude Float
instance Magnitude Double
instance Magnitude CFloat
instance Magnitude CDouble

-- * RealNumber membership
-------------------------------------------------------------------------------
instance Formattable a => Formattable (RealNumber a) where
    format (RealNumber r) = format r

instance Formattable a => Show (RealNumber a) where
    show = string . format
    
-- * Rational membership
-------------------------------------------------------------------------------
instance Newtype (RationalNumber a)    

instance Integral a => Rational (RationalNumber a) where
    numerator (RationalNumber r) = numerator' r
    {-# INLINE numerator #-} 

    denominator (RationalNumber r) = denominator' r
    {-# INLINE denominator #-} 
    
instance (Formattable a,Integral a) => Formattable (RationalNumber a) where
    format (RationalNumber q) = format (numerator' q) <> FSlash <> format (denominator' q)

instance (Formattable a, Integral a) => Show (RationalNumber a) where
    show = string . format

-- * Complex membership
-------------------------------------------------------------------------------
instance Negatable a => ComplexNumeric (ComplexNumber a) where
    conjugate (ComplexNumber (r,i)) = complex (r, negate i)
    re (ComplexNumber (r, _)) = r    
    im (ComplexNumber (_,i)) = i
    
instance (Multiplicative a) => LeftAction a (ComplexNumber a) where
    k *. (ComplexNumber (x,y) ) = complex (k * x, k * y)

instance (Multiplicative a) => RightAction (ComplexNumber a) a where
    (ComplexNumber (x,y) ) .* k = complex (x * k, y * k)
    
instance (Multiplicative a, Additive a, Subtractive a) => Multiplicative (ComplexNumber a) where
    mul (ComplexNumber (a,b)) (ComplexNumber (c,d)) = complex (a*c - b*d, a*d + b*c)

instance (Multiplicative a, Additive a, Subtractive a, Divisive a) => Invertible (ComplexNumber a) where
    invert (ComplexNumber (a,b)) = complex(a / bottom, b / bottom) where
        bottom = a*a + b*b

instance (Multiplicative a, Additive a, Subtractive a, Divisive a) => Divisive (ComplexNumber a) where
    div (ComplexNumber (a,b)) (ComplexNumber (c,d)) = complex ( (a*c + b*d)/bottom, (b*c - a*d) / bottom) where
        bottom = c*c + d*d
    
instance (Formattable a, Negatable a, Signable a) => Formattable (ComplexNumber a) where
    format (ComplexNumber (r,i)) = format r <> pad symbol <> format val <> Il where
        s = sign i
        (symbol, val) = (ifelse (s == Negative) Dash Plus, ifelse (s == Negative) (negate i) i)
        
instance (Negatable a, Formattable a, Signable a) => Show(ComplexNumber a) where
    show  = string . format

instance (Additive a, Multiplicative a, Subtractive a) => LeftDistributive (ComplexNumber a)

instance (Additive a, Multiplicative a, Subtractive a) => RightDistributive (ComplexNumber a) 
        
instance Formattable a => Formattable (ExtendedReal a)  where
    format (BoundedReal r) = format r
    format (InfiniteReal i) = format i
    format (UndefinedReal i) = format i

instance Formattable a => Show (ExtendedReal a) where
    show = string . format
