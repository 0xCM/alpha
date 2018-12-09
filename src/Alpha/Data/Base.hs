{-# LANGUAGE NoStarIsType #-}
-----------------------------------------------------------------------------
-- | Base Data.* modules
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE MagicHash #-}

module Alpha.Data.Base
(
    Arg, ArgMin, ArgMax,
    FiniteBits(..),
    ByteString,    
    Bool(..),(&&), (||), not, otherwise,
    Coercible(..), Coercion(..),
    Monoid(..),
    Dual(..), Endo(..), All(..), Any(..), Sum(..),
    Groupoid(..),
    String, Text,
    HashSet,HashMap, Tree,     Forest,

    Map, associate,
    Either(..),
    IsString,
    TotalOrder, Ord, Ordering,
    Show(..), Read(..),
    ($),undefined, id, const,    
    Semigroup(..), Min(..), Max(..), First(..), Last(..),
    Data(..),
    Type,
    Char,chr,intToDigit,
    Functor(..),
    Default(..),
    Eq(..),
    Seq,
    Set,
    Bag,
    Stream,
    Traversable(..),
    Typeable(..), Proxy(..),
    Maybe(..), isNothing, fromJust,
    Ix(..),
    Int,Int8,Int16,Int32,Int64,
    Word,Word8,Word16,Word32,Word64,
    Integer, Natural,
    CDouble,CFloat,
    Num, signum,    
    Rational(..), Ratio(..),numerator, denominator,
    fromInteger,
    Fractional, recip, fromRational, frac,
    Floating, pi, exp, log, sin, cos, asin, acos, atan, sinh, cosh, asinh, acosh, atanh, sqrt,
    RealFloat, floatRadix, floatDigits, floatRange, decodeFloat, encodeFloat, exponent, significand, scaleFloat, isNaN, isIEEE, isInfinite, isDenormalized, isNegativeZero, atan2,
    RealFrac, properFraction, truncate, round, ceiling, floor,
    Double , Double#, Float, Float#,
    Generic(..),
    Generic1(..),    
    NonEmpty(..),
    IsList(..),   
    Bounded, minBound, maxBound,
    Enum, fromEnum, toEnum,
    Integral, Real, fromIntegral,
    KnownNat, SomeNat, Nat, SomeSymbol, KnownSymbol, Symbol, SymbolPair,
    symbolVal, symbolVal',
    nat, natg, proxy, 
    Semigroupoid(..), 
    divMod, (/%),
    Storable, poke, peek, sizeOf, alignment,
    Ptr, castPtr,
    type (+), type (-), type (*), Mod(..), type (%), Div(..), type (/), type (^),        
    quotRem, (/%%),
    seq,
    OrderedEnum(..),
    readEither, readMaybe
    
    
    

)
where

import Data.Bool(Bool(..), (&&), (||), not, otherwise)
import Data.Bits(Bits(..),FiniteBits(..))
import Data.Char(Char,chr,intToDigit)
import Data.Coerce(Coercible(..),coerce)
import Data.Data(Data(..))
import Data.Monoid(Monoid(mempty, mappend,mconcat))
import Data.Monoid(Dual(..), Endo(..), All(..), Any(..),  Sum(..),Alt(..))
import Data.Default(Default(def))
import Data.Either(Either(..))
import Data.Eq(Eq(..),(==),(/=))
import Data.Foldable(Foldable, foldMap, fold, foldr, foldr', foldl, foldl')
import Data.Functor(Functor(..))
import Data.Groupoid(Groupoid(..))
import Data.HashSet(HashSet)
import Data.HashMap.Strict(HashMap)
import Data.Int(Int,Int8,Int16,Int32,Int64)
import Data.Ix(Ix(..))
import Data.Kind(Type)
import Data.Map.Strict(Map)
import Data.Maybe(Maybe(..),isNothing,fromJust)
import Data.Monoid(Monoid(..))
import Data.Ord(Ord, Ordering)
import Data.Proxy
import Data.Ratio(Ratio(..))
import Data.String(String,IsString)
import Data.Sequence(Seq)
import Data.Semigroup(Semigroup(..), Min(..), Max(..), First(..), Last(..),(<>), sconcat)
import Data.Text(Text)
import Text.Read(readEither,readMaybe)
import Data.Traversable(Traversable(..))
import Data.Typeable(Typeable(..),Proxy(..))
import Data.Word(Word,Word8,Word16,Word32,Word64)
import Data.Function(const)
import Data.Semigroup(Arg,ArgMin,ArgMax)
import Data.Semigroupoid(Semigroupoid(..))
import Data.Set(Set)
import Data.Stream.Infinite
import Data.List.NonEmpty(NonEmpty((:|)) )
import Data.Coerce(Coercible(..))
import Data.Type.Coercion
import Data.Tree(Tree,Forest)
import Data.ByteString(ByteString)
import GHC.Float
import GHC.Num
import Data.Ratio(Rational(..))
import GHC.Real(Fractional(..),RealFrac(..),numerator,denominator,divMod,quotRem)
import GHC.TypeLits(type (+), type (-), type (*), type (^), Mod(..), Div(..))
import GHC.TypeLits(KnownNat, SomeNat, Nat,natVal, natVal', symbolVal, symbolVal',SomeSymbol,KnownSymbol)
import GHC.Generics(Generic(..),Generic1(..))
import GHC.Types(Symbol)
import GHC.Show(Show(..))
import GHC.Read(Read(..))
import GHC.Base(($),undefined, id, (.),seq)
import GHC.Num(Num,Integer)
import GHC.Exts(IsList(..))
import GHC.Enum(Enum, fromEnum,toEnum, Bounded,minBound,maxBound)
import GHC.Real(Real, Integral, mod, fromIntegral)
import Foreign.C(CDouble,CFloat)
import Foreign.Storable(Storable,poke, peek, sizeOf, alignment)
import Foreign.Ptr (Ptr, castPtr)
import Text.Show
import Data.String(IsString(..))
import qualified Data.Map.Strict as Map
import qualified Data.MultiSet as MS

type Bag a = MS.MultiSet a

type SymbolPair s t = (KnownSymbol s, KnownSymbol t)

-- Synonym for combined Ord and Enum constraints
type OrderedEnum a = (Enum a, Ord a)    

type TotalOrder a = Ord a

-- | Folds a structure projected into a 'Monoid' by a supplied function
foldby :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
foldby = foldMap

-- | Produces an associative array for a list of key-value pairs
associate::(Ord k) => [(k,v)] -> Map k v
associate = Map.fromList

-- Modulus infix operator synonm
type (%) m n = Mod m n

-- Division infix operator synonm
type (/) m n = Div m n
infixl 7 /

(/%)::(Integral n) => n -> n -> (n,n)
(/%) = divMod
{-# INLINE (/%) #-}
infixl 5 /%

(/%%)::(Integral n) => n -> n -> (n,n)
(/%%) = quotRem
{-# INLINE (/%%) #-}
infixl 5 /%%

(!=)::(Eq a) => a -> a -> Bool
(!=) = (/=)
{-# INLINE (!=) #-}
infixl 4 !=    

-- | Infix alias for the base fractional division operator (/)
frac::(Fractional a) => a -> a -> a
frac = (/)
{-# INLINE frac #-}

proxy:: Proxy n
proxy = Proxy
{-# INLINE proxy #-}

-- | Computes the 'Int' value corresponding to a type-level nat
nat::forall m. KnownNat m => Int
nat = fromIntegral $ natVal (proxy @m) 
{-# INLINE nat #-}

-- | Computes the (generic) integral value corresponding to a type-level nat
natg::forall m i. (KnownNat m, Integral i) => i
natg = fromIntegral $ natVal (proxy @m) 
{-# INLINE natg #-}

