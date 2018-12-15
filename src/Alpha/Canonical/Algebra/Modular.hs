{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE OverloadedLists #-}

module Alpha.Canonical.Algebra.Modular
(
    Modulo(..), Modular(..), Bimodular(..),

    Zn, BasedInt,
    Base(..),
    zN, modulus, residues,residue,
    based, base2, base10, base16,
    base, digits,

    (/%), (/%%)
) where
import Alpha.Base
import Alpha.Native
import Alpha.Canonical.Text
import Alpha.Canonical.Element
import Alpha.Canonical.Operators
import Alpha.Canonical.Relations
import Alpha.Canonical.Algebra.Divisive
import Alpha.Canonical.Algebra.Subtractive
import Alpha.Canonical.Algebra.Multiplicative
import Alpha.Canonical.Algebra.Additive
import Alpha.Canonical.Algebra.Nullary
import Alpha.Canonical.Algebra.Unital
import qualified Data.Vector as Vector

import qualified Data.List as List
import qualified Data.Set as Set

-- Represents a numeric base
newtype Base b = Base Natural
    deriving(Show,Eq,Ord,Num)

-- | Represents an integer with a particular base
data BasedInt (b::Nat) i = BasedInt !i

-- | Represents the ring of integers mod n
data Zn (n::Nat) = Zn Integer
    deriving (Eq, Ord)

data Residue (n::Nat) = Residue (Zn n) Integer
    deriving (Eq, Ord)

-- Represents a sequence of digits expressed with respect to a
-- particular basis    
newtype Digits b = Digits [Char]    

-- Represents a family of type pairs that support a notion of the first 
-- type 'mod' the second type. Intended to represent to the result of the 
-- modulus operation on integers, a paritioning of a set by a subset or
-- more generally, quotient groups and similar
type family Modulo a b

-- Homogenous Modulo type
-------------------------------------------------------------------------------
type instance Modulo Int Int = Int
type instance Modulo Int8 Int8 = Int8
type instance Modulo Int16 Int16 = Int16
type instance Modulo Int32 Int32 = Int32
type instance Modulo Int64 Int64 = Int64
type instance Modulo Integer Integer = Integer
type instance Modulo Word Word = Word
type instance Modulo Word8 Word8 = Word8
type instance Modulo Word16 Word16 = Word16
type instance Modulo Word32 Word32 = Word32
type instance Modulo Word64 Word64 = Word64
type instance Modulo Natural Natural = Natural
type instance Element (Zn n) = Residue n

class (Nullary a, Eq a, Divisive a) => Modular a where
    -- | Calculates the remainder of dividing the first operand by the second
    rem::BinaryOperator a

    mod::BinaryOperator a

    -- | Infix synonym for 'rem'
    (%)::BinaryOperator a
    (%) = rem
    {-# INLINE (%) #-}
    infix 8 %

    -- | Infix synonym for 'mod'
    (%%)::BinaryOperator a
    (%%) = mod
    {-# INLINE (%%) #-}
    infix 8 %%

    
    -- Determines whether m is evenly Divisive by n
    divides::a -> a -> Bool
    divides m n = m % n == zero
    {-# INLINE divides #-}      

    -- Calculates the points within the interval that are
    -- Divisive by n
    modpart::(Ix a) => (a, a) -> a -> [a]
    modpart (min,max) n 
        = range' (min, max) 
        |> (<$>) (\j -> case divides j n of True -> j; _ -> zero)
        |> List.filter (\j -> j /= zero)
    {-# INLINE modpart #-}      

class Bimodular a b where
    -- | Calculates the remainder of dividing the first operand by the second
    bimod::a -> b -> Modulo a b

    -- | Infix synonym for 'bimod'
    (>%<)::a -> b -> Modulo a b
    (>%<) = bimod
    {-# INLINE (>%<) #-}
    infix 8 >%<

(/%)::(Integral n) => n -> n -> (n,n)
(/%) = quotRem
{-# INLINE (/%) #-}
infixl 5 /%

(/%%)::(Integral n) => n -> n -> (n,n)
(/%%) = divMod
{-# INLINE (/%%) #-}
infixl 5 /%%
        
-- | Constructs an integer with a specified base
based::forall (n::Nat) i. (Integral i) => i -> BasedInt n i
based i = BasedInt i

-- | Constructs a base-2 integer
base2::Integer -> BasedInt 2 Integer
base2 = based @2

-- | Constructs a base-10 integer
base10::Integer -> BasedInt 10 Integer
base10 = based @10

-- | Constructs a base-16 integer
base16::Integer -> BasedInt 16 Integer
base16 = based @16

-- Constructs a representation for the ring of integers mod n
zN::forall n. KnownNat n => Zn n
zN = Zn $ natg @n

modulus::forall n.KnownNat n => Zn n -> Integer
modulus (Zn n) = mod' n (natg @n)

residue::(KnownNat n) => Integer -> Residue n 
residue m = Residue (zN) m

--rebase::forall m n i. (KnownNat m, KnownNat n, Integral i) => BasedInt n i -> BasedInt m i

-- See https://github.com/TikhonJelvis/modular-arithmetic/blob/master/src/Data/Modular.hs

-- | The canonical least residues modulo n
-- See https://en.wikipedia.org/wiki/Modular_arithmetic for terminology
residues::KnownNat n => Zn n -> [Residue n]    
residues (Zn n) = residue <$> [0..(n-1)] 

base::forall b. KnownNat b => Base b
base = Base $ natg @b

--encoding64::Vector.Vector Char
--See https://en.wikipedia.org/wiki/Base64 for table
    
encoding::Vector.Vector Char
encoding = 
    ['0','1','2','3','4','5','6','7','8','9',
     'A','B','C','D','E','F','G','H','I','J',
     'K','L','M','N','O','P','Q','R','S','T',
     'U','V','W','X','Y','Z']

encode::(Integral i) => i -> Char
encode i = encoding Vector.! (fromIntegral i)

digits::forall b i. (KnownNat b, Integral i) => i -> Digits b
digits i = Digits $ recurse (quotRem i b) []  
    where
        b = natg @b
        recurse (n,d) r = seq c $
            case n of
            0 -> r'
            _ -> recurse (quotRem n b) r' 
            where
                c  = encode d
                r' = c : r            

instance KnownNat n => Show (Zn n) where
    show _ = "Z/" <> nn where
        nn = show $ nat @n

instance forall b i. (KnownNat b, Formattable i) => Formattable (BasedInt b i) where
    format (BasedInt i) = format i <> b where
        b = format (nat @b) |> parenthetical |> spaced

instance forall b i. (KnownNat b, Formattable i) => Show(BasedInt b i) where
    show = string . format
    
instance forall b. KnownNat b => Formattable (Digits b ) where
    format (Digits n) = format n <> b where 
        b = format (nat @b) |> parenthetical |> spaced

instance forall b. KnownNat b => Show (Digits b ) where
    show = string . format

instance  KnownNat n => Set (Zn n)

instance  KnownNat n => Membership (Zn n) where    
    members s = residues s |> Set.fromList

instance KnownNat n => Show (Residue n) where
    show (Residue zN m) = (show m) <> " " <> (show zN)

instance KnownNat n => Additive (Residue n) where
    add (Residue zN a) (Residue _ b) 
        = residue( (a + b) % (modulus zN) )

instance KnownNat n => Nullary (Residue n) where        
    zero = residue 0

instance KnownNat n => Subtractive (Residue n) where
    sub (Residue zN a) (Residue _ b) 
        = residue( (a - b) % (modulus zN) )
    
instance KnownNat n => Multiplicative (Residue n) where
    mul (Residue zN a) (Residue _ b) 
        = residue( (a * b) % (modulus zN) )    

instance KnownNat n => Unital (Residue n) where        
    one = residue 1

instance Modular Natural where 
    rem = rem'
    {-# INLINE rem #-}
    mod = mod'
    {-# INLINE mod #-}

instance Modular Integer where 
    rem = rem'
    {-# INLINE rem #-}
    mod = mod'
    {-# INLINE mod #-}

instance Modular Int where 
    rem = rem'
    {-# INLINE rem #-}
    mod = mod'    
    {-# INLINE mod #-}

instance Modular Int8 where 
    rem = rem'
    {-# INLINE rem #-}
    mod = mod'
    {-# INLINE mod #-}

instance Modular Int16 where 
    rem = rem'
    {-# INLINE rem #-}
    mod = mod'
    {-# INLINE mod #-}

instance Modular Int32 where 
    rem = rem'
    {-# INLINE rem #-}
    mod = mod'    
    {-# INLINE mod #-}

instance Modular Int64 where 
    rem = rem'
    {-# INLINE rem #-}
    mod = mod'
    {-# INLINE mod #-}

instance Modular Word where 
    rem = rem'
    {-# INLINE rem #-}
    mod = mod'
    {-# INLINE mod #-}

instance Modular Word8 where 
    rem = rem'
    {-# INLINE rem #-}
    mod = mod'
    {-# INLINE mod #-}

instance Modular Word16 where 
    rem = rem'
    {-# INLINE rem #-}
    mod = mod'
    {-# INLINE mod #-}

instance Modular Word32 where 
    rem = rem'
    {-# INLINE rem #-}
    mod = mod'
    {-# INLINE mod #-}

instance Modular Word64 where 
    rem = rem'
    {-# INLINE rem #-}
    mod = mod'
    {-# INLINE mod #-}
