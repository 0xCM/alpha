-----------------------------------------------------------------------------
-- | Defines the elementary number-related API surface
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Alpha.Data.Numbers
(    
    SizedInt(..), 
    SizedWord(..),
    Integral(quot, rem, mod, quotRem, divMod),
    Num(signum),

    --intn, wordn,    
    int8, int16, int32, int64,
    word8, word16, word32, word64,
    divides, modpart,
    fromIntegral,
    fromInteger,
    sub',

    numerator, denominator, ratio, 
    rational, fractional

)
where
import Data.Word 
import Data.Int
import Data.Bits 
import Data.Kind(Type)
import GHC.Num hiding (abs) 
import GHC.Natural
import GHC.Real
import GHC.TypeLits
import GHC.Float(double2Float,float2Double)
import Foreign.C(CDouble,CFloat)
import Prelude(realToFrac)

import qualified GHC.Num as N
import qualified Data.List as L
import qualified Data.Text as Text
import qualified Data.Ratio as DR
import Data.Ratio(Ratio(..))

import Numeric
import Alpha.Base hiding(zero)
import Alpha.Text.Format
import Alpha.Canonical hiding((+),(*),(-),(^),(^^),(**), range)

-- | Constructs a list of Int values from a list of Integral values
ints::(ToInt n) => [n] -> [Int]
ints src = fmap int src

-- | Constructs a list of Word values from a list of Integral values
words::(ToWord n) => [n] -> [Word]
words src = fmap word src

-- | Constructs a list of Integer values from a list of Integral values
integers::(ToInteger n) => [n] -> [Integer]
integers src = fmap integer src

-- | Constructs a 'Int8' from an integral value
int8::(Integral n) => n -> Int8
int8 n = convert n

-- | Constructs a 'Int16' from an integral value
int16::(Integral n) => n -> Int16
int16 n = convert n

-- | Constructs a 'Int32' from an integral value
int32::(Integral n) => n -> Int32
int32 n = convert n

-- | Constructs a 'Int64' from an integral value
int64::(Integral n) => n -> Int64
int64 n = convert n

-- | Constructs a 'Word8' from an integral value
word8::(Integral n) => n -> Word8
word8 n = convert n

-- | Constructs a 'Word16' from an integral value
word16::(Integral n) => n -> Word16
word16 n = convert n

-- | Constructs a 'Word32' from an integral value
word32::(Integral n) => n -> Word32
word32 n = convert n

-- | Constructs a 'Word64' from an integral value
word64::(Integral n) => n -> Word64
word64 n = convert n
{-# INLINE word64 #-}

-- Forms the 'Ratio' of two integral values        
ratio::(Integral n) => n -> n -> Ratio n
ratio m n =  (DR.%) m n
{-# INLINE ratio #-}

-- Forms a 'Rational' number from a 'Real number
rational::(Real r) => r -> Rational
rational = toRational
{-# INLINE rational #-} 

fractional::(Real r, Fractional f) => r -> f
fractional = realToFrac
{-# INLINE fractional #-} 

-- Determines whether m is evenly divisible by n
divides::(Integral a) => a -> a -> Bool
divides m n = (m `mod` n) == 0
{-# INLINE divides #-}      

-- Calculates the points within the interval that are
-- divisible by n
modpart::(Integral a, Ix a) => (a, a) -> a -> [a]
modpart (min,max) n 
    = range (min, max) 
      |> fmap (\j -> case divides j n of True -> j; _ -> 0)
      |> L.filter (\j -> j /= 0)
{-# INLINE modpart #-}      

-- | Constructs a Num-typed 0 value      
zed::(Num a) => a
zed = 0
{-# INLINE zed #-}

-- | Constructs a Num-typed 1 value      
unity::(Num a) => a
unity = 1
{-# INLINE unity #-}

sub'::(Num a) => a -> a -> a
sub' x y = x - y
{-# INLINE sub' #-}

abs'::(Num a) => a -> a
abs' = N.abs
{-# INLINE abs' #-}

div'::(Integral a) => a -> a -> a
div' = (GHC.Real.div)
{-# INLINE div' #-}

divf::(Fractional a) => a -> a -> a
divf = (GHC.Real./)
{-# INLINE divf #-}

negate'::(Num a) => a -> a
negate' = N.negate
{-# INLINE negate' #-}

instance (Integral a, Num b) => Convertible a b where
    convert = fromIntegral

-- instance (SignedIntegral a) => Invertible a a where 
--     invert x = zed - x
   
-- Subtractive
-------------------------------------------------------------------------------
instance Subtractive Natural where 
    sub = (-)
    {-# INLINE sub #-}
instance Subtractive Integer where 
    sub = (-)
    {-# INLINE sub #-}
instance Subtractive Int where 
    sub = (-)
    {-# INLINE sub #-}
instance Subtractive Int8 where 
    sub = (-)
    {-# INLINE sub #-}
instance Subtractive Int16 where 
    sub = (-)
    {-# INLINE sub #-}
instance Subtractive Int32 where 
    sub = (-)
    {-# INLINE sub #-}
instance Subtractive Int64 where 
    sub = (-)
    {-# INLINE sub #-}
instance Subtractive Word where 
    sub = (-)
    {-# INLINE sub #-}
instance Subtractive Word8 where 
    sub = (-)
    {-# INLINE sub #-}
instance Subtractive Word16 where 
    sub = (-)
    {-# INLINE sub #-}
instance Subtractive Word32 where 
    sub = (-)
    {-# INLINE sub #-}
instance Subtractive Word64 where 
    sub = (-)
    {-# INLINE sub #-}
instance (Integral a) => Subtractive (Ratio a) where 
    sub = (-)
    {-# INLINE sub #-}    
instance Subtractive Float where 
    sub = (-)
    {-# INLINE sub #-}
instance Subtractive Double where 
    sub = (-)
    {-# INLINE sub #-}
instance Subtractive CFloat where 
    sub = (-)
    {-# INLINE sub #-}
instance Subtractive CDouble where 
    sub = (-)
    {-# INLINE sub #-}

-- Additive
-------------------------------------------------------------------------------
instance Additive Natural where 
    add = (+)
    {-# INLINE add #-}
instance Additive Integer where 
    add = (+)
    {-# INLINE add #-}
instance Additive Int where 
    add = (+)
    {-# INLINE add #-}
instance Additive Int8 where 
    add = (+)
    {-# INLINE add #-}
instance Additive Int16 where 
    add = (+)
    {-# INLINE add #-}
instance Additive Int32 where 
    add = (+)
    {-# INLINE add #-}
instance Additive Int64 where 
    add = (+)
    {-# INLINE add #-}
instance Additive Word where 
    add = (+)
    {-# INLINE add #-}
instance Additive Word8 where 
    add = (+)
    {-# INLINE add #-}
instance Additive Word16 where 
    add = (+)
    {-# INLINE add #-}
instance Additive Word32 where 
    add = (+)
    {-# INLINE add #-}
instance Additive Word64 where 
    add = (+)
    {-# INLINE add #-}
instance (Integral a) => Additive (Ratio a) where 
    add = (+)
    {-# INLINE add #-}    
instance Additive Float where 
    add = (+)
    {-# INLINE add #-}
instance Additive Double where 
    add = (+)
    {-# INLINE add #-}
instance Additive CFloat where 
    add = (+)
    {-# INLINE add #-}
instance Additive CDouble where 
    add = (+)
    {-# INLINE add #-}

-- Semigroup
-------------------------------------------------------------------------------
instance Semigroup Natural where 
    (<>) = (+)
    {-# INLINE (<>) #-}
instance Semigroup Integer where 
    (<>) = (+)
    {-# INLINE (<>) #-}
instance Semigroup Int where 
    (<>) = (+)
    {-# INLINE (<>) #-}
instance Semigroup Int8 where 
    (<>) = (+)
    {-# INLINE (<>) #-}
instance Semigroup Int16 where 
    (<>) = (+)
    {-# INLINE (<>) #-}
instance Semigroup Int32 where 
    (<>) = (+)
    {-# INLINE (<>) #-}
instance Semigroup Int64 where 
    (<>) = (+)
    {-# INLINE (<>) #-}
instance Semigroup Word where 
    (<>) = (+)
    {-# INLINE (<>) #-}
instance Semigroup Word8 where 
    (<>) = (+)
    {-# INLINE (<>) #-}
instance Semigroup Word16 where 
    (<>) = (+)
    {-# INLINE (<>) #-}
instance Semigroup Word32 where 
    (<>) = (+)
    {-# INLINE (<>) #-}
instance Semigroup Word64 where 
    (<>) = (+)
    {-# INLINE (<>) #-}
instance Integral a => Semigroup (Ratio a) where 
    (<>) = (+)
    {-# INLINE (<>) #-}    
instance Semigroup Float where 
    (<>) = (+)
    {-# INLINE (<>) #-}
instance Semigroup Double where 
    (<>) = (+)
    {-# INLINE (<>) #-}
instance Semigroup CFloat where 
    (<>) = (+)
    {-# INLINE (<>) #-}
instance Semigroup CDouble where 
    (<>) = (+)
    {-# INLINE (<>) #-}

-- Multiplicative
-------------------------------------------------------------------------------
instance Multiplicative Natural where 
    mul = (*)
    {-# INLINE mul #-}
instance Multiplicative Integer where 
    mul = (*)
    {-# INLINE mul #-}
instance Multiplicative Int where 
    mul = (*)
    {-# INLINE mul #-}
instance Multiplicative Int8 where 
    mul = (*)
    {-# INLINE mul #-}
instance Multiplicative Int16 where 
    mul = (*)
    {-# INLINE mul #-}
instance Multiplicative Int32 where 
    mul = (*)
    {-# INLINE mul #-}
instance Multiplicative Int64 where 
    mul = (*)
    {-# INLINE mul #-}
instance Multiplicative Word where 
    mul = (*)
    {-# INLINE mul #-}
instance Multiplicative Word8 where 
    mul = (*)
    {-# INLINE mul #-}
instance Multiplicative Word16 where 
    mul = (*)
    {-# INLINE mul #-}
instance Multiplicative Word32 where 
    mul = (*)
    {-# INLINE mul #-}
instance Multiplicative Word64 where 
    mul = (*)
    {-# INLINE mul #-}
instance (Integral a) => Multiplicative (Ratio a) where 
    mul = (*)
    {-# INLINE mul #-}    
instance Multiplicative Float where 
    mul = (*)
    {-# INLINE mul #-}
instance Multiplicative Double where 
    mul = (*)
    {-# INLINE mul #-}
instance Multiplicative CFloat where 
    mul = (*)
    {-# INLINE mul #-}
instance Multiplicative CDouble where 
    mul = (*)
    {-# INLINE mul #-}

-- Absolutist
-------------------------------------------------------------------------------
instance Absolutist Natural where 
    abs = N.abs 
    {-# INLINE abs #-}
instance Absolutist Integer where 
    abs = N.abs 
    {-# INLINE abs #-}
instance Absolutist Int where 
    abs = N.abs 
    {-# INLINE abs #-}
instance Absolutist Int8 where 
    abs = N.abs 
    {-# INLINE abs #-}
instance Absolutist Int16 where 
    abs = N.abs 
    {-# INLINE abs #-}
instance Absolutist Int32 where 
    abs = N.abs 
    {-# INLINE abs #-}
instance Absolutist Int64 where 
    abs = N.abs 
    {-# INLINE abs #-}
instance Absolutist Word where 
    abs = N.abs 
    {-# INLINE abs #-}
instance Absolutist Word8 where 
    abs = N.abs 
    {-# INLINE abs #-}
instance Absolutist Word16 where 
    abs = N.abs 
    {-# INLINE abs #-}
instance Absolutist Word32 where 
    abs = N.abs 
    {-# INLINE abs #-}
instance Absolutist Word64 where 
    abs = N.abs 
    {-# INLINE abs #-}
instance Integral a => Absolutist (Ratio a) where 
    abs = N.abs 
    {-# INLINE abs #-}    
instance Absolutist Float where 
    abs = N.abs 
    {-# INLINE abs #-}
instance Absolutist Double where 
    abs = N.abs 
    {-# INLINE abs #-}
instance Absolutist CFloat where 
    abs = N.abs 
    {-# INLINE abs #-}
instance Absolutist CDouble where 
    abs = N.abs 
    {-# INLINE abs #-}

-- Nullary
-------------------------------------------------------------------------------
instance Nullary Natural where 
    zero = zed
    {-# INLINE zero #-}
instance Nullary Integer where 
    zero = zed
    {-# INLINE zero #-}
instance Nullary Int where 
    zero = zed
    {-# INLINE zero #-}
instance Nullary Int8 where 
    zero = zed
    {-# INLINE zero #-}
instance Nullary Int16 where 
    zero = zed
    {-# INLINE zero #-}
instance Nullary Int32 where 
    zero = zed
    {-# INLINE zero #-}
instance Nullary Int64 where 
    zero = zed
    {-# INLINE zero #-}
instance Nullary Word where 
    zero = zed
    {-# INLINE zero #-}
instance Nullary Word8 where 
    zero = zed
    {-# INLINE zero #-}
instance Nullary Word16 where 
    zero = zed
    {-# INLINE zero #-}
instance Nullary Word32 where 
    zero = zed
    {-# INLINE zero #-}
instance Nullary Word64 where 
    zero = zed
    {-# INLINE zero #-}
instance (Integral a) => Nullary (Ratio a) where 
    zero = zed
    {-# INLINE zero #-}    
instance Nullary Float where 
    zero = zed
    {-# INLINE zero #-}
instance Nullary Double where 
    zero = zed
    {-# INLINE zero #-}
instance Nullary CFloat where 
    zero = zed
    {-# INLINE zero #-}
instance Nullary CDouble where 
    zero = zed
    {-# INLINE zero #-}

-- Monoid
-------------------------------------------------------------------------------
instance Monoid Natural where 
    mempty = zero
    {-# INLINE mempty #-}
instance Monoid Integer where 
    mempty = zero
    {-# INLINE mempty #-}
instance Monoid Int where 
    mempty = zero
    {-# INLINE mempty #-}
instance Monoid Int8 where 
    mempty = zero
    {-# INLINE mempty #-}
instance Monoid Int16 where 
    mempty = zero
    {-# INLINE mempty #-}
instance Monoid Int32 where 
    mempty = zero
    {-# INLINE mempty #-}
instance Monoid Int64 where 
    mempty = zero
    {-# INLINE mempty #-}
instance Monoid Word where 
    mempty = zero
    {-# INLINE mempty #-}
instance Monoid Word8 where 
    mempty = zero
    {-# INLINE mempty #-}
instance Monoid Word16 where 
    mempty = zero
    {-# INLINE mempty #-}
instance Monoid Word32 where 
    mempty = zero
    {-# INLINE mempty #-}
instance Monoid Word64 where 
    mempty = zero
    {-# INLINE mempty #-}
instance (Integral a) => Monoid (Ratio a) where 
    mempty = zero
    {-# INLINE mempty #-}    
instance Monoid Float where 
    mempty = zero
    {-# INLINE mempty #-}
instance Monoid Double where 
    mempty = zero
    {-# INLINE mempty #-}
instance Monoid CFloat where 
    mempty = zero
    {-# INLINE mempty #-}
instance Monoid CDouble where 
    mempty = zero
    {-# INLINE mempty #-}

-- Unital
-------------------------------------------------------------------------------
instance Unital Natural where 
    one = unity
    {-# INLINE one #-}
instance Unital Integer where 
    one = unity
    {-# INLINE one #-}
instance Unital Int where 
    one = unity
    {-# INLINE one #-}
instance Unital Int8 where 
    one = unity
    {-# INLINE one #-}
instance Unital Int16 where 
    one = unity
    {-# INLINE one #-}
instance Unital Int32 where 
    one = unity
    {-# INLINE one #-}
instance Unital Int64 where 
    one = unity
    {-# INLINE one #-}
instance Unital Word where 
    one = unity
    {-# INLINE one #-}
instance Unital Word8 where 
    one = unity
    {-# INLINE one #-}
instance Unital Word16 where 
    one = unity
    {-# INLINE one #-}
instance Unital Word32 where 
    one = unity
    {-# INLINE one #-}
instance Unital Word64 where 
    one = unity
    {-# INLINE one #-}
instance (Integral a) => Unital (Ratio a) where 
    one = unity
    {-# INLINE one #-}    
instance Unital Float where 
    one = unity
    {-# INLINE one #-}
instance Unital Double where 
    one = unity
    {-# INLINE one #-}
instance Unital CFloat where 
    one = unity
    {-# INLINE one #-}
instance Unital CDouble where 
    one = unity
    {-# INLINE one #-}

-- Invertible
-------------------------------------------------------------------------------
instance Invertible Int where 
    invert x = zero - x
    {-# INLINE invert #-}
instance Invertible Int8 where 
    invert x = zero - x
    {-# INLINE invert #-}
instance Invertible Int16 where 
    invert x = zero - x
    {-# INLINE invert #-}
instance Invertible Int32 where 
    invert x = zero - x
    {-# INLINE invert #-}
instance Invertible Int64 where 
    invert x = zero - x
    {-# INLINE invert #-}
instance Invertible Integer where 
    invert x = zero - x
    {-# INLINE invert #-}    
instance (Integral a) => Invertible (Ratio a) where 
    invert x = zero - x
    {-# INLINE invert #-}    
instance Invertible Float where 
    invert x = zero - x
    {-# INLINE invert #-}
instance Invertible Double where 
    invert x = zero - x
    {-# INLINE invert #-}
instance Invertible CFloat where 
    invert x = zero - x
    {-# INLINE invert #-}
instance Invertible CDouble where 
    invert x = zero - x
    {-# INLINE invert #-}

-- Negatable 
-------------------------------------------------------------------------------
instance Negatable Natural Integer where 
    negate x = zero - (integer x)
    {-# INLINE negate #-}
instance Negatable Integer Integer where 
    negate = negateInteger
    {-# INLINE negate #-}
instance Negatable Int Int where 
    negate = negate'
    {-# INLINE negate #-}
instance Negatable Int8 Int8 where 
    negate = negate'
    {-# INLINE negate #-}
instance Negatable Int16 Int16 where 
    negate = negate'
    {-# INLINE negate #-}
instance Negatable Int32 Int32 where 
    negate = negate'
    {-# INLINE negate #-}
instance Negatable Int64 Int64 where 
    negate = negate'
    {-# INLINE negate #-}
instance Negatable Word Int where 
    negate x = zero- (int x)
    {-# INLINE negate #-}
instance Negatable Word8 Int8 where 
    negate x = zero - (int8 x)
    {-# INLINE negate #-}
instance Negatable Word16 Int16 where 
    negate x = zero - (int16 x)
    {-# INLINE negate #-}
instance Negatable Word32 Int32 where 
    negate x = zero - (int32 x)
    {-# INLINE negate #-}
instance Negatable Word64 Int64 where 
    negate x = zero - (int64 x)
    {-# INLINE negate #-}
instance (Integral a) => Negatable (Ratio a) (Ratio a) where 
    negate x = zero - x
    {-# INLINE negate #-}    
instance Negatable Float Float where 
    negate = negate'
    {-# INLINE negate #-}
instance Negatable Double Double where 
    negate = negate'
    {-# INLINE negate #-}
instance Negatable CFloat CFloat where 
    negate = negate'
    {-# INLINE negate #-}
instance Negatable CDouble CDouble where 
    negate = negate'
    {-# INLINE negate #-}

-- Divisible
-------------------------------------------------------------------------------
instance Divisible Natural where 
    div = div'
    {-# INLINE div #-}
instance Divisible Integer where 
    div = div'
    {-# INLINE div #-}
instance Divisible Int where 
    div = div'
    {-# INLINE div #-}
instance Divisible Int8 where 
    div = div'
    {-# INLINE div #-}
instance Divisible Int16 where 
    div = div'
    {-# INLINE div #-}
instance Divisible Int32 where 
    div = div'
    {-# INLINE div #-}
instance Divisible Int64 where 
    div = div'
    {-# INLINE div #-}
instance Divisible Word where 
    div = div'
    {-# INLINE div #-}
instance Divisible Word8 where 
    div = div'
    {-# INLINE div #-}
instance Divisible Word16 where 
    div = div'
    {-# INLINE div #-}
instance Divisible Word32 where 
    div = div'
    {-# INLINE div #-}
instance Divisible Word64 where 
    div = div'
    {-# INLINE div #-}
instance (Integral a) => Divisible (Ratio a) where 
    div = divf
    {-# INLINE div #-}    
instance Divisible Float where 
    div = divf
    {-# INLINE div #-}
instance Divisible Double where 
    div = divf
    {-# INLINE div #-}
instance Divisible CFloat where 
    div = divf
    {-# INLINE div #-}
instance Divisible CDouble where 
    div = divf
    {-# INLINE div #-}

-- FromDouble
-------------------------------------------------------------------------------
instance FromDouble Natural where 
    fromDouble = truncate
    {-# INLINE fromDouble #-}    
instance FromDouble Integer where 
    fromDouble = truncate
    {-# INLINE fromDouble #-}    
instance FromDouble Int where 
    fromDouble = truncate
    {-# INLINE fromDouble #-}    
instance FromDouble Int8 where 
    fromDouble = truncate
    {-# INLINE fromDouble #-}    
instance FromDouble Int16 where 
    fromDouble = truncate
    {-# INLINE fromDouble #-}    
instance FromDouble Int32 where 
    fromDouble = truncate
    {-# INLINE fromDouble #-}    
instance FromDouble Int64 where 
    fromDouble = truncate
    {-# INLINE fromDouble #-}    
instance FromDouble Word where 
    fromDouble = truncate
    {-# INLINE fromDouble #-}    
instance FromDouble Word8 where 
    fromDouble = truncate
    {-# INLINE fromDouble #-}    
instance FromDouble Word16 where 
    fromDouble = truncate
    {-# INLINE fromDouble #-}    
instance FromDouble Word32 where 
    fromDouble = truncate
    {-# INLINE fromDouble #-}    
instance FromDouble Word64 where 
    fromDouble = truncate
    {-# INLINE fromDouble #-}    
instance FromDouble Float where 
    fromDouble = double2Float
    {-# INLINE fromDouble #-}    
instance FromDouble Double where 
    fromDouble = id
    {-# INLINE fromDouble #-}    
instance FromDouble CFloat where 
    fromDouble = realToFrac
    {-# INLINE fromDouble #-}    
instance FromDouble CDouble where 
    fromDouble = realToFrac
    {-# INLINE fromDouble #-}    

-- To Double
-------------------------------------------------------------------------------
instance ToDouble Natural where 
    double = fromIntegral
    {-# INLINE double #-}    
instance ToDouble Integer where 
    double = fromIntegral
    {-# INLINE double #-}    
instance ToDouble Int where 
    double = fromIntegral
    {-# INLINE double #-}    
instance ToDouble Int8 where 
    double = fromIntegral
    {-# INLINE double #-}    
instance ToDouble Int16 where 
    double = fromIntegral
    {-# INLINE double #-}    
instance ToDouble Int32 where 
    double = fromIntegral
    {-# INLINE double #-}    
instance ToDouble Int64 where 
    double = fromIntegral
    {-# INLINE double #-}    
instance ToDouble Word where 
    double= fromIntegral
    {-# INLINE double #-}    
instance ToDouble Word8 where 
    double = fromIntegral
    {-# INLINE double #-}    
instance ToDouble Word16 where 
    double = fromIntegral
    {-# INLINE double #-}    
instance ToDouble Word32 where 
    double = fromIntegral
    {-# INLINE double #-}    
instance ToDouble Word64 where 
    double = fromIntegral
    {-# INLINE double #-}    
instance (Integral a, ToDouble a) => ToDouble (Ratio a) where 
    double x = divf n d where
        n = double <| numerator x
        d = double <| denominator x        
    {-# INLINE double #-}        
instance ToDouble Float where 
    double = float2Double
    {-# INLINE double #-}    
instance ToDouble Double where 
    double = id
    {-# INLINE double #-}    
instance ToDouble CFloat where 
    double = realToFrac
    {-# INLINE double #-}    
instance ToDouble CDouble where 
    double = realToFrac
    {-# INLINE double #-}    

-- ToInt
-------------------------------------------------------------------------------
instance ToInt Natural where 
    int = fromIntegral
    {-# INLINE int #-}
instance ToInt Integer where 
    int = fromIntegral
    {-# INLINE int #-}
instance ToInt Int where 
    int = id
    {-# INLINE int #-}
instance ToInt Int8 where 
    int = fromIntegral
    {-# INLINE int #-}
instance ToInt Int16 where 
    int = fromIntegral
    {-# INLINE int #-}
instance ToInt Int32 where 
    int = fromIntegral
    {-# INLINE int #-}
instance ToInt Int64 where 
    int = fromIntegral
    {-# INLINE int #-}
instance ToInt Word where 
    int= fromIntegral
    {-# INLINE int #-}
instance ToInt Word8 where 
    int = fromIntegral
    {-# INLINE int #-}
instance ToInt Word16 where 
    int = fromIntegral
    {-# INLINE int #-}
instance ToInt Word32 where 
    int = fromIntegral
    {-# INLINE int #-}
instance ToInt Word64 where 
    int = fromIntegral
    {-# INLINE int #-}
instance ToInt Float where 
    int = truncate
    {-# INLINE int #-}
instance ToInt Double where 
    int = fromDouble
    {-# INLINE int #-}
instance ToInt CFloat where 
    int = truncate
    {-# INLINE int #-}
instance ToInt CDouble where 
    int = truncate
    {-# INLINE int #-}

-- FromInt
-------------------------------------------------------------------------------
instance FromInt Natural where 
    fromInt = fromIntegral
    {-# INLINE fromInt #-}
instance FromInt Integer where 
    fromInt = fromIntegral
    {-# INLINE fromInt #-}
instance FromInt Int where 
    fromInt = id
    {-# INLINE fromInt #-}
instance FromInt Int8 where 
    fromInt = fromIntegral
    {-# INLINE fromInt #-}
instance FromInt Int16 where 
    fromInt = fromIntegral
    {-# INLINE fromInt #-}
instance FromInt Int32 where 
    fromInt = fromIntegral
    {-# INLINE fromInt #-}
instance FromInt Int64 where 
    fromInt = fromIntegral
    {-# INLINE fromInt #-}
instance FromInt Word where 
    fromInt= fromIntegral
    {-# INLINE fromInt #-}
instance FromInt Word8 where 
    fromInt = fromIntegral
    {-# INLINE fromInt #-}
instance FromInt Word16 where 
    fromInt = fromIntegral
    {-# INLINE fromInt #-}
instance FromInt Word32 where 
    fromInt = fromIntegral
    {-# INLINE fromInt #-}
instance FromInt Word64 where 
    fromInt = fromIntegral
    {-# INLINE fromInt #-}
instance FromInt Float where 
    fromInt = realToFrac
    {-# INLINE fromInt #-}
instance FromInt Double where 
    fromInt = realToFrac
    {-# INLINE fromInt #-}
instance FromInt CFloat where 
    fromInt = realToFrac
    {-# INLINE fromInt #-}
instance FromInt CDouble where 
    fromInt = realToFrac
    {-# INLINE fromInt #-}

-- ToInteger
-------------------------------------------------------------------------------
instance ToInteger Natural where 
    integer = fromIntegral
    {-# INLINE integer #-}
instance ToInteger Integer where 
    integer = id
    {-# INLINE integer #-}
instance ToInteger Int where 
    integer = fromIntegral
    {-# INLINE integer #-}
instance ToInteger Int8 where 
    integer = fromIntegral
    {-# INLINE integer #-}
instance ToInteger Int16 where 
    integer = fromIntegral
    {-# INLINE integer #-}
instance ToInteger Int32 where 
    integer = fromIntegral
    {-# INLINE integer #-}
instance ToInteger Int64 where 
    integer = fromIntegral
    {-# INLINE integer #-}
instance ToInteger Word where 
    integer= fromIntegral
    {-# INLINE integer #-}
instance ToInteger Word8 where 
    integer = fromIntegral
    {-# INLINE integer #-}
instance ToInteger Word16 where 
    integer = fromIntegral
    {-# INLINE integer #-}
instance ToInteger Word32 where 
    integer = fromIntegral
    {-# INLINE integer #-}
instance ToInteger Word64 where 
    integer = fromIntegral
    {-# INLINE integer #-}
instance ToInteger Float where 
    integer = truncate
    {-# INLINE integer #-}
instance ToInteger Double where 
    integer = fromDouble
    {-# INLINE integer #-}
instance ToInteger CFloat where 
    integer = truncate
    {-# INLINE integer #-}
instance ToInteger CDouble where 
    integer = truncate
    {-# INLINE integer #-}

-- ToWord
-------------------------------------------------------------------------------
instance ToWord Natural where 
    word = fromIntegral
    {-# INLINE word #-}
instance ToWord Integer where 
    word = fromIntegral
    {-# INLINE word #-}
instance ToWord Int where 
    word = fromIntegral
    {-# INLINE word #-}
instance ToWord Int8 where 
    word = fromIntegral
    {-# INLINE word #-}
instance ToWord Int16 where 
    word = fromIntegral
    {-# INLINE word #-}
instance ToWord Int32 where 
    word = fromIntegral
    {-# INLINE word #-}
instance ToWord Int64 where 
    word = fromIntegral
    {-# INLINE word #-}
instance ToWord Word where 
    word = id
    {-# INLINE word #-}
instance ToWord Word8 where 
    word = fromIntegral
    {-# INLINE word #-}
instance ToWord Word16 where 
    word = fromIntegral
    {-# INLINE word #-}
instance ToWord Word32 where 
    word = fromIntegral
    {-# INLINE word #-}
instance ToWord Word64 where 
    word = fromIntegral
    {-# INLINE word #-}
instance ToWord Float where 
    word = truncate
    {-# INLINE word #-}
instance ToWord Double where 
    word = fromDouble
    {-# INLINE word #-}
instance ToWord CFloat where 
    word = truncate
    {-# INLINE word #-}
instance ToWord CDouble where 
    word = truncate
    {-# INLINE word #-}

-- NaturallyPowered
-------------------------------------------------------------------------------
instance NaturallyPowered Natural where 
    pow = (^)
    {-# INLINE pow #-}
instance NaturallyPowered Integer where 
    pow = (^)
    {-# INLINE pow #-}
instance NaturallyPowered Int where 
    pow = (^)
    {-# INLINE pow #-}
instance NaturallyPowered Int8 where 
    pow = (^)
    {-# INLINE pow #-}
instance NaturallyPowered Int16 where 
    pow = (^)
    {-# INLINE pow #-}
instance NaturallyPowered Int32 where 
    pow = (^)
    {-# INLINE pow #-}
instance NaturallyPowered Int64 where 
    pow = (^)
    {-# INLINE pow #-}
instance NaturallyPowered Word where 
    pow = (^)
    {-# INLINE pow #-}
instance NaturallyPowered Word8 where 
    pow = (^)
    {-# INLINE pow #-}
instance NaturallyPowered Word16 where 
    pow = (^)
    {-# INLINE pow #-}
instance NaturallyPowered Word32 where 
    pow = (^)
    {-# INLINE pow #-}
instance NaturallyPowered Word64 where 
    pow = (^)
    {-# INLINE pow #-}
instance (Integral n) => NaturallyPowered (Ratio n) where 
    pow = (^)
    {-# INLINE pow #-}    
instance NaturallyPowered Float where 
    pow = (^)
    {-# INLINE pow #-}
instance NaturallyPowered Double where 
    pow = (^)
    {-# INLINE pow #-}
instance NaturallyPowered CFloat where 
    pow = (^)
    {-# INLINE pow #-}
instance NaturallyPowered CDouble where 
    pow = (^)
    {-# INLINE pow #-}

-- ApproximatelyPowered
-------------------------------------------------------------------------------
instance ApproximatelyPowered Float where 
    powa = (**)
    {-# INLINE powa #-}
instance ApproximatelyPowered Double where 
    powa = (**)
    {-# INLINE powa #-}
instance ApproximatelyPowered CFloat where 
    powa = (**)
    {-# INLINE powa #-}
instance ApproximatelyPowered CDouble where 
    powa = (**)
    {-# INLINE powa #-}

-- IntegrallyPowered
-------------------------------------------------------------------------------
instance (Integral n) => IntegrallyPowered (Ratio n) where 
    powi = (^^)
    {-# INLINE powi #-}
instance IntegrallyPowered Float where 
    powi = (^^)
    {-# INLINE powi #-}
instance IntegrallyPowered Double where 
    powi = (^^)
    {-# INLINE powi #-}
instance IntegrallyPowered CFloat where 
    powi = (^^)
    {-# INLINE powi #-}
instance IntegrallyPowered CDouble where 
    powi = (^^)
    {-# INLINE powi #-}

instance Unsigned Natural
instance Unsigned Word
instance Unsigned Word8
instance Unsigned Word16
instance Unsigned Word32
instance Unsigned Word64

instance Signed Integer
instance Signed Int
instance Signed Int8
instance Signed Int16
instance Signed Int32
instance Signed Int64
instance Signed Float
instance Signed Double
instance Signed CFloat
instance Signed CDouble
    
instance SignedIntegral Int
instance SignedIntegral Int8
instance SignedIntegral Int16
instance SignedIntegral Int32
instance SignedIntegral Int64
instance SignedIntegral Integer

instance UnsignedIntegral Word
instance UnsignedIntegral Word8
instance UnsignedIntegral Word16
instance UnsignedIntegral Word32
instance UnsignedIntegral Word64
instance UnsignedIntegral Natural

instance Group Integer
instance Group Int
instance Group Int8
instance Group Int16
instance Group Int32
instance Group Int64
instance (Integral a) => Group (Ratio a)

instance AbelianSemigroup Integer
instance AbelianSemigroup Int
instance AbelianSemigroup Int8
instance AbelianSemigroup Int16
instance AbelianSemigroup Int32
instance AbelianSemigroup Int64
instance (Integral a) => AbelianSemigroup (Ratio a)

instance AbelianGroup Integer
instance AbelianGroup Int
instance AbelianGroup Int8
instance AbelianGroup Int16
instance AbelianGroup Int32
instance AbelianGroup Int64
instance (Integral a) => AbelianGroup (Ratio a)

instance Ring Integer    
instance Ring Int
instance Ring Int8
instance Ring Int16
instance Ring Int32
instance Ring Int64
instance (Integral a) => Ring (Ratio a)

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
instance (Integral a, TotalOrder a) => Numeric (Ratio a)
instance Numeric Float
instance Numeric Double
instance Numeric CFloat
instance Numeric CDouble
