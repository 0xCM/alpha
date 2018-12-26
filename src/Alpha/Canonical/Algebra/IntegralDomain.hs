
module Alpha.Canonical.Algebra.IntegralDomain
(
    module X,
    IntegralDomain(..),
    GcdDomain(..),
    Ufd(..),
    Pid(..),
    Eud(..)
    

) where
import Alpha.Canonical.Relations
import Alpha.Canonical.Algebra.Ring as X
import qualified Data.List as List
import Math.NumberTheory.Primes(factorise) 
import Math.NumberTheory.Euclidean(extendedGCD)

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

class IntegralDomain a => GcdDomain a where
    gcd :: a -> a -> a
    default gcd::(Integral a) => a -> a -> a
    gcd = gcd'
    {-# INLINE gcd #-}

    lcm :: a -> a -> a
    default lcm::(Integral a) => a -> a -> a
    lcm = lcm'
    {-# INLINE lcm #-}

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
        