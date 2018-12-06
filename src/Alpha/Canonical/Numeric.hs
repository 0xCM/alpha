-----------------------------------------------------------------------------
-- | Fundamental abstractions related to numeric representation
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Numeric 
(
    FromDouble(..), ToDouble(..),
    FromInt(..), ToInt(..),
    ToWord(..), FromWord(..),
    ToInteger(..),
    Doubly(..),
    Numeric(..),
    SizedInt(..),
    SizedWord(..),
    Absolutist(..),
    Negatable(..),
    Signed(..),
    Unsigned(..),
    SignedIntegral(..),
    UnsignedIntegral(..),
    NaturallyPowered(..),
    IntegrallyPowered(..),
    ApproximatelyPowered(..),
    Counted(..),


)
where
import Alpha.Base
import Alpha.Canonical.Algebra
import Alpha.Canonical.Relations

-- | Characterizies a type whose values can be materialized from 'Double' values
class FromDouble d where
    -- | Converts a 'Double' value to a 'd' value
    fromDouble::Double -> d

-- | Characterizies a type whose values can be converted to 'Double' values
class ToDouble d where
    -- / Converts a 'd' value to a 'Double' value
    double::d -> Double

-- | Characterizies a type whose values can be converted to machine-sized 'Int' values
class ToInt d where
    int::d -> Int  

-- | Characterizies a type whose values can be materialized from machine-sized 'Int' values
class FromInt a where
    fromInt::Int -> a
    
-- | Characterizies a type whose values can be converted to arbitrary-sized 'Integer' values
class ToInteger d where
    integer::d -> Integer

-- | Characterizies a type whose values can be converted to machine-sized 'Word' values
class ToWord d where
    word::d -> Word

-- | Characterizies a type whose values can be materialized from machine-sized 'Word' values
class FromWord a where
    fromWord::Word -> a

-- | Characterizies a type whose values can be converted to/from 'Double' values    
type Doubly a = (ToDouble a, FromDouble a)

class Absolutist a where
    abs::a -> a
    

class (TotalOrder a, Subtractive a, Semigroup a, Multiplicative a, Nullary a, Unital a, Monoid a, Absolutist a, Divisible a, Real a) => Numeric a where
    num::a -> a
    num = id
    {-# INLINE num #-}

type family SizedInt (n::Nat) = r | r -> n  where
    SizedInt 8 = Int8
    SizedInt 16 = Int16
    SizedInt 32 = Int32
    SizedInt 64 = Int64

type family SizedWord (n::Nat) = r | r -> n where
    SizedWord 8 = Word8
    SizedWord 16 = Word16
    SizedWord 32 = Word32
    SizedWord 64 = Word64    

-- / Characterizes types for which unary negation may be defined
class Negatable a where
    type Negated a
    type Negated a = a

    -- | Negates the operand
    negate::a -> Negated a

-- Classifies unsigned numeric types
class Unsigned a where

-- Classifies signednumeric types    
class Signed a where

-- Identifies signed integral types
class (Integral i, Signed i) => SignedIntegral i where

-- Classifies usigned integral types    
class (Integral i, Unsigned i) => UnsignedIntegral i where
    
class NaturallyPowered a where
    pow::(UnsignedIntegral p) => a -> p -> a

    (^)::(UnsignedIntegral p) => a -> p -> a
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
    
-- | Defines membership predicated on the ability to be counted by an existential machine
class Counted a where
    -- | Counts the number of items within the purview of the subject
    count::(Integral n) => a -> n
