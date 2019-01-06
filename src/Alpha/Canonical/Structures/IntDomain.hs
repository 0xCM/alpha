-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE BangPatterns #-}
module Alpha.Canonical.Structures.IntDomain
(
    module X,
    IntegralDomain(..),
    IntegralNumeric(..),
    NaturalNumeric(..),
    Rational(..),
    IntDomain(..),
    GcdDomain(..),
    Ufd(..),
    Pid(..),
    Eud(..),
    Residue(..),
    modN,
    Zn, zN, even,odd
    

) where
import Alpha.Canonical.Algebra as X
import Alpha.Canonical.Structures.Ring as X
import qualified Data.List as List
import Math.NumberTheory.Primes(factorise) 
import Math.NumberTheory.Euclidean(extendedGCD)


modN::forall n i. (KnownNat n, Integral i) => Residue n i
modN = Residue (natg @n)

data Residue (n::Nat) i = Residue i
    deriving (Eq, Ord)

-- | Represents the ring of integers mod n
data Zn (n::Nat) i = Zn [Residue n i]
    deriving (Eq, Ord)

type instance Individual (Zn n i) = Residue n i 
type instance Individual (Residue n i) = Residue n i 
type instance Summed (Zn n i) (Zn n i) = Zn n i
type instance Summed (Residue n i) (Residue n i) = Residue n i
    
type IntDomain i = (Integral i, IntegralDomain i)

    
-- | An integral domain is a commutative ring such such that
-- for all elements a != 0 && b != 0, a*b != 0
class (Divisive a, CommutativeRing a) => IntegralDomain a where
    quotRem::a -> a -> (a, a)
    default quotRem::(Integral a) => a -> a -> (a, a)
    quotRem = quotRem'
    {-# INLINE quotRem #-}

    divMod::a -> a -> (a, a)
    default divMod::(Integral a) => a -> a -> (a, a)
    divMod = divMod'
    {-# INLINE divMod #-}

    quot::a -> a -> a
    default quot::(Integral a) => a -> a -> a
    quot = quot'
    {-# INLINE quot #-}

    rem :: a -> a -> a
    default rem::(Integral a) => a -> a -> a
    rem = rem'
    {-# INLINE rem #-}

    mod :: a -> a -> a
    default mod::(Integral a) => a -> a -> a
    mod = mod'
    {-# INLINE mod #-}

    -- | Infix synonym for 'rem'
    (%)::a -> a -> a
    default (%)::(Integral a) => a -> a -> a
    (%) = rem
    {-# INLINE (%) #-}
    infix 8 %
    
    residue::forall n .KnownNat n => a -> Residue n a
    default residue::forall n. (KnownNat n, Integral a) => a -> Residue n a
    residue i = Residue (i % (natg @ n))

    -- Determines whether m is evenly Divisive by n
    divides::a -> a -> Bool
    default divides::(Integral a) => a -> a -> Bool
    divides m n = m % n == zero
    {-# INLINE divides #-}      

    -- Calculates the points within the interval that are
    -- Divisive by n
    modpart::(Ix a) => (a, a) -> a -> [a]
    default modpart::(Ix a, Integral a) => (a,a) -> a -> [a]
    modpart (min,max) n 
        = range' (min, max) 
        |> (<$>) (\j -> case divides j n of True -> j; _ -> zero)
        |> List.filter (\j -> j /= zero)
    {-# INLINE modpart #-}      

even::(IntegralDomain i) => i -> Bool
even i = i % (one + one) == zero

odd::(IntegralDomain i) => i -> Bool
odd = not . even

-- | The canonical least residues modulo n
-- See https://en.wikipedia.org/wiki/Modular_arithmetic
zN::forall n i. (KnownNat n, IntDomain i) => Zn n i 
zN = Zn $ residue <$> [0..(n-1)] 
    where n = natg @n

class IntegralDomain a => GcdDomain a where
    gcd :: a -> a -> a
    default gcd::(Integral a) => a -> a -> a
    gcd = gcd'
    {-# INLINE gcd #-}

    lcm :: a -> a -> a
    default lcm::(Integral a) => a -> a -> a
    lcm = lcm'
    {-# INLINE lcm #-}

type IntegralNumeric a = (Numeric a, Integral a, IntegralDomain a)

-- | Classifies unsigned integral numeric values    
type NaturalNumeric a = (IntegralNumeric a, UnsignedIntegral a) 

newtype Factorization a = Factorization [(a,Int)]    
    deriving (Eq,Ord)

-- | Characterizes a unique factorization domain    
class (GcdDomain a) => Ufd a where
    factor::a -> Factorization a
    default factor::(Integral a) => a -> Factorization a        
    factor a = (\(n,p) -> (fromIntegral n,p)) <$> factors  |> Factorization where
        factors = factorise (fromIntegral a)

-- | Characterizes a principal ideal domain        
class Ufd a => Pid a where

-- | Characterizes a Euclidean Domain    
-- See https://en.wikipedia.org/wiki/Euclidean_domain
class Pid a => Eud a  where


instance IntegralDomain Integer
instance IntegralDomain Int
instance IntegralDomain Int8
instance IntegralDomain Int16
instance IntegralDomain Int32
instance IntegralDomain Int64
instance IntegralDomain Natural where 
instance IntegralDomain Word where 
instance IntegralDomain Word8 where 
instance IntegralDomain Word16 where 
instance IntegralDomain Word32 where 
instance IntegralDomain Word64 where             
    
instance GcdDomain Integer
instance GcdDomain Int
instance GcdDomain Int8
instance GcdDomain Int16
instance GcdDomain Int32
instance GcdDomain Int64
instance GcdDomain Natural where 
instance GcdDomain Word where 
instance GcdDomain Word8 where 
instance GcdDomain Word16 where 
instance GcdDomain Word32 where 
instance GcdDomain Word64 where             
    
instance Ufd Integer where
instance Ufd Int where
instance Ufd Int8 where
instance Ufd Int16
instance Ufd Int32
instance Ufd Int64
instance Ufd Natural where 
instance Ufd Word where 
instance Ufd Word8 where 
instance Ufd Word16 where 
instance Ufd Word32 where 
instance Ufd Word64 where             

instance Pid Integer where
instance Pid Int where
instance Pid Int8 where
instance Pid Int16
instance Pid Int32
instance Pid Int64
instance Pid Natural where 
instance Pid Word where 
instance Pid Word8 where 
instance Pid Word16 where 
instance Pid Word32 where 
instance Pid Word64 where             
        
instance Eud Integer where
instance Eud Int where
instance Eud Int8 where
instance Eud Int16
instance Eud Int32
instance Eud Int64
instance Eud Natural where 
instance Eud Word where 
instance Eud Word8 where 
instance Eud Word16 where 
instance Eud Word32 where 
instance Eud Word64 where             

        
instance forall n i. (KnownNat n, Formattable i) => Formattable (Zn n i) where
    format _ = "Z/" <> nn where
        nn = format $ nat @n

instance forall n i. (KnownNat n, Formattable i) => Show (Zn n i) where
    show = string . format
        
instance forall n i. (KnownNat n, Show i) => Show (Residue n i) where
    show (Residue m) = (show m) <> " Z/" <> show (natg @n)

instance forall n i. (KnownNat n, IntDomain i) => Additive (Residue n i) where
    add (Residue a) (Residue b) 
        = residue( (a + b) % ( natg @n) )

instance forall n i. (KnownNat n, IntDomain i) => Nullary (Residue n i) where        
    zero = residue zero

instance forall n i.  (KnownNat n, IntDomain i) => Subtractive (Residue n i) where
    sub (Residue a) (Residue b) 
        = residue( (a - b) % (natg @n) )

instance forall n i. (KnownNat n, IntDomain i) => Negatable (Residue n i) where
    negate r = zero - r
                
instance forall n i. (KnownNat n, IntDomain i) => Multiplicative (Residue n i) where
    mul (Residue a) (Residue b) 
        = residue( (a * b) % (natg @n) )    
                        
instance forall n i. (KnownNat n, IntDomain i) => Unital (Residue n i) where        
    one = residue one

instance forall n i. (KnownNat n, IntDomain i) => Discrete (Zn n i) where        
    individuals (Zn x) = x
    
instance forall n i. (KnownNat n, IntDomain i) => Finite (Zn n i)
    
instance (KnownNat n, IntDomain i) => Discrete (Residue n i) where
    individuals _ = individuals $ zN @n    

instance (KnownNat n, IntDomain i) => Finite (Residue n i)
    

instance (KnownNat n, IntDomain i) => AbelianGroup (Residue n i)        
instance (KnownNat n, IntDomain i) => FiniteAbelianGroup (Residue n i)        
instance (KnownNat n, IntDomain i) => LeftDistributive (Residue n i)
instance (KnownNat n, IntDomain i) => RightDistributive (Residue n i)
instance (KnownNat n, IntDomain i) => Ring (Residue n i)    
    

class (IntegralNumeric a) => Rational a where
    numerator::Ratio a -> a
    denominator::Ratio a -> a

-- | Extended gcd function in a Euclidean domain
-- Implementation taken from arithmoi
-- egcd a b = (s,t,d) where d = gcd(a,b) = sa + tb
egcd::(Eud a, Num a) => a -> a -> (a,a,a)
egcd a b = (d, x * signum a, y * signum b) 
    where
    (d, x, y) = eGCD 0 1 1 0 (abs' a) (abs' b)
    eGCD !n1 o1 !n2 o2 r s
        | s == 0    = (r, o1, o2)
        | otherwise = case r `quotRem` s of
                        (q, t) -> eGCD (o1 - q*n1) n1 (o2 - q*n2) n2 s t
