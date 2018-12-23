module Alpha.Canonical.Numeric.Types
(
    URatio, uratio,
    UFloat, ufloat,
    UDouble, udouble, 
    UCFloat, ucfloat,
    UCDouble,ucdouble, 
    Unsigned(..),
)
where
import Alpha.Base
import Alpha.Native
import Alpha.Canonical.Text
import Alpha.Canonical.Algebra

-- | A ratio sans sign
newtype URatio a = URatio (Ratio a)
    deriving(Eq, Ord, Show, Formattable, Num, Fractional,Real,Divisive,Generic)

-- | A float sans sign
newtype UFloat = UFloat Float
    deriving(Eq, Ord, Show, Formattable, Num, Fractional, Floating,Real,Divisive,Generic)
instance Newtype(UFloat)

-- | A double sans sign    
newtype UDouble = UDouble Double
    deriving(Eq, Ord, Show, Formattable, Num, Fractional, Floating,Real,Divisive,Generic)
instance Newtype(UDouble)

-- | A cfloat sans sign
newtype UCFloat = UCFloat CFloat
    deriving(Eq, Ord, Show, Formattable, Num, Fractional, Floating,Real,Divisive,Generic)
instance Newtype(UCFloat)

-- | A cdouble sans sign    
newtype UCDouble = UCDouble CDouble
    deriving(Eq, Ord, Show, Formattable, Num, Fractional, Floating,Real,Divisive,Generic)
instance Newtype(UCDouble)

-- Projects signed typed onto their unsigned counterparts and applies an identity projection otherwise
type family Unsigned a
type instance Unsigned Natural = Natural
type instance Unsigned Word = Word
type instance Unsigned Word8 = Word8
type instance Unsigned Word16 = Word16
type instance Unsigned Word32 = Word32
type instance Unsigned Word64 = Word64
type instance Unsigned Integer = Natural
type instance Unsigned Int = Word
type instance Unsigned (Ratio a) = URatio a
type instance Unsigned Int8 = Word8
type instance Unsigned Int16 = Word16
type instance Unsigned Int32 = Word32
type instance Unsigned Int64 = Word64
type instance Unsigned Float = UFloat
type instance Unsigned Double = UDouble
type instance Unsigned CFloat = UCFloat
type instance Unsigned CDouble = UCDouble
type instance Unsigned UFloat = UFloat
type instance Unsigned UDouble = UDouble
type instance Unsigned UCFloat = UCFloat
type instance Unsigned UCDouble = UCDouble

newtype Fraction a = Fraction (Ratio a)

-- | Constructs a 'Uloat' value     
ufloat::Float -> UFloat
ufloat x = UFloat (abs' x)
{-# INLINE ufloat #-}

-- | Constructs a 'UDouble' value 
udouble::Double -> UDouble
udouble x = UDouble (abs' x)
{-# INLINE udouble #-}

-- | Constructs a 'Uloat' value     
ucfloat::CFloat -> UCFloat
ucfloat x = UCFloat (abs' x)
{-# INLINE ucfloat #-}

-- | Constructs a 'UDouble' value 
ucdouble::CDouble -> UCDouble
ucdouble x = UCDouble (abs' x)
{-# INLINE ucdouble #-}

-- | Constructs a 'URatio' value 
uratio::(Integral a)=> Ratio a -> URatio a
uratio x = URatio (abs' x)
{-# INLINE uratio #-}
    
instance Unsignable URatio
instance Unsignable UFloat
instance Unsignable UDouble
instance Unsignable UCFloat
instance Unsignable UCDouble
