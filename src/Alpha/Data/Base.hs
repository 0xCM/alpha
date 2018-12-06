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
    HashSet,HashMap,
    Map, associate,
    Either(..),
    IsString,
    Ord, Ordering,
    ($),
    undefined, id, const,    
    Semigroup(..), Min(..), Max(..), First(..), Last(..),
    Data(..),
    Type,
    Char,chr,intToDigit,
    Functor(..),
    Default(..),
    Eq(..),
    Seq,
    Set,
    Traversable(..),
    Typeable(..), Proxy(..),
    Maybe(..), isNothing, fromJust,
    Ix(..),
    Int,Int8,Int16,Int32,Int64,
    Word,Word8,Word16,Word32,Word64,
    Integer, Natural,
    Num, signum,

    Rational(..),
    Fractional(..),
    Floating(..), RealFloat(..), RealFrac(..),
    Double , Double#, Float, Float#,
    Generic(..),
    Generic1(..),    
    IsList(..),    
    Bounded,minBound,maxBound,
    Enum,fromEnum,toEnum,
    Integral, Real, mod, fromIntegral,
    (%),
    type (+), type (-), type (*), Mod, type (%), Div, type (/),
    KnownNat, SomeNat, Nat, SomeSymbol, KnownSymbol, natVal, natVal', symbolVal, symbolVal',
    Symbol, 
    Show(..), Read(..)
    
)
where
import Data.Bool(Bool(..), (&&), (||), not, otherwise)
import Data.Bits(FiniteBits(..))
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
import Data.String(String,IsString)
import Data.Sequence(Seq)
import Data.Semigroup(Semigroup(..), Min(..), Max(..), First(..), Last(..),(<>), sconcat)
import Data.Text(Text)
import Data.Traversable(Traversable(..))
import Data.Typeable(Typeable(..),Proxy(..))
import Data.Word(Word,Word8,Word16,Word32,Word64)
import Data.Function(const)
import Data.Semigroup(Arg,ArgMin,ArgMax)
import Data.Set(Set)
import Data.Coerce(Coercible(..))
import Data.Type.Coercion
import Data.ByteString(ByteString)
import GHC.Float
import GHC.Num
import Data.Ratio(Rational(..))
import GHC.Real(Fractional(..),RealFrac(..))
import GHC.TypeLits(type (+), type (-), type (*), type (^), Mod, Div)
import GHC.TypeLits(KnownNat, SomeNat, Nat,natVal, natVal', symbolVal, symbolVal',SomeSymbol,KnownSymbol)
import GHC.Generics(Generic(..),Generic1(..))
import GHC.Types(Symbol)
import GHC.Show(Show(..))
import GHC.Read(Read(..))
import GHC.Base(($),undefined, id)
import GHC.Num(Num,Integer)
import GHC.Exts(IsList(..))
import GHC.Enum(Enum, fromEnum,toEnum, Bounded,minBound,maxBound)
import GHC.Real(Real, Integral, mod, fromIntegral)

import qualified Data.Map.Strict as Map

-- | Folds a structure projected into a 'Monoid' by a supplied function
foldby :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
foldby = foldMap

-- | Produces an associative array for a list of key-value pairs
associate::(Ord k) => [(k,v)] -> Map k v
associate = Map.fromList

-- Modulus infix operator synonm
type (%) m n = Mod m n

-- Infix operator synonym for 'mod' function
(%)::(Integral n) => n -> n -> n
(%) = mod

infixl 7 %

-- Division infix operator synonm
type (/) m n = Div m n

infixl 7 /

