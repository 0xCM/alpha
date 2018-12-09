-----------------------------------------------------------------------------
-- | Newtype generics; derived from https://github.com/sjakobi/newtype-generics
-- Copyright   :  See license file #Darius Jahandarie
-- License     :  Per license
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

-- The 'Newtype' typeclass and related functions.
-- Primarily pulled from Conor McBride's Epigram work. Some examples:

-- >>> ala Sum foldMap [1,2,3,4]
-- 10

-- >>> ala Endo foldMap [(+1), (+2), (subtract 1), (*2)] 3
-- 8

-- >>> under2 Min (<>) 2 1
-- 1

-- >>> over All not (All False)
-- All {getAll = True)

-- This wrapage includes 'Newtype' instances for all the (non-GHC\/foreign)
-- newtypes in base (as seen in the examples).
-- However, there are neat things you can do with this with
-- /any/ newtype and you should definitely define your own 'Newtype'
-- instances for the power of this library.
-- For example, see @ala Cont traverse@, with the proper 'Newtype' instance for Cont.
-- You can easily define new instances for your newtypes with the help of GHC.Generics

-- {-# LANGUAGE DeriveGeneric #-}
-- import GHC.Generics

-- newtype Example = Example Int
-- deriving (Generic)

-- instance Newtype Example
-- This avoids the use of Template Haskell (TH) to get new instances.

module Alpha.Data.Newtype
  ( Newtype(..)
  , op
  , ala
  , ala'
  , under
  , over
  , under2
  , over2
  , underF
  , overF
  ) where

import Control.Applicative
import Control.Arrow
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Fixed
import Data.Monoid
import Data.Ord
import qualified Data.Semigroup
import Data.Semigroup (Min(..), Max(..), WrappedMonoid(..), Option(..))
import GHC.Generics
import Alpha.Base hiding(Alt,Last,First)


-- | Given a newtype @n@, we will always have the same unwrapped type @o@,
-- meaning we can represent this with a fundep @n -> o@.
--
-- Any instance of this class just needs to let @wrap@ equal to the newtype's
-- constructor, and let @unwrap@ destruct the newtype with pattern matching.
{-class Newtype n o | n -> o where-}
  {-wrap :: o -> n-}
  {-unwrap :: n -> o-}

-- Generic Newtype
class GNewtype n where
  type GO n :: *
  gwrap   :: GO n -> n p
  gunwrap :: n p  -> GO n

-- We only need one instance, if these generic functions are only to work for
-- newtypes, as these have a fixed form. For example, for a newtype X = Y,
-- Rep X = D1 ... (C1 ... (S1 ... (K1 ... Y)))
instance GNewtype (D1 d (C1 c (S1 s (K1 i a)))) where
  type GO (D1 d (C1 c (S1 s (K1 i a)))) = a
  gwrap   x                     = M1 (M1 (M1 (K1 x)))
  gunwrap (M1 (M1 (M1 (K1 x)))) = x

-- | Synonyn for newtype wrapping operator  
type Wrapper a b = a -> b

-- | Synonyn for newtype unwrapping operator  
type Unwrapper a b = b -> a

-- Original Newtype class, extended with generic defaults (trivial) and deprived
-- of the second type argument (less trivial, as it involves a type family with
-- a default, plus an equality constraint for the related type family in
-- GNewtype). We do get rid of MultiParamTypeClasses and FunctionalDependencies,
-- though.

-- | As long as the type @n@ is an instance of Generic, you can create an instance
-- with just @instance Newtype n@
class Newtype n where
  type O n :: *
  type O n = GO (Rep n)

  wrap   :: Wrapper (O n) n
  default wrap :: (Generic n, GNewtype (Rep n), O n ~ GO (Rep n)) => O n -> n
  wrap = to . gwrap

  unwrap :: Unwrapper(O n) n
  default unwrap :: (Generic n, GNewtype (Rep n), O n ~ GO (Rep n)) => n -> O n
  unwrap = gunwrap . from

-- |
-- This function serves two purposes:
--
-- 1. Giving you the unwrap of a newtype without you needing to remember the name.
--
-- 2. Showing that the first parameter is /completely ignored/ on the value level,
--    meaning the only reason you pass in the constructor is to provide type
--    information.  Typeclasses sure are neat.
--
-- >>> op Identity (Identity 3)
-- 3
op :: (Newtype n, o ~ O n ) => Unwrapper n o -> Wrapper n o
op _ = unwrap

-- | Given a "wraper" and a \"higher order function\" (/hof/),
-- it handles the wraping and unwraping, and just sends you back a regular old
-- function, with the type varying based on the /hof/ you passed.
--
-- The reason for the signature of the /hof/ is due to 'ala' not caring about structure.
-- To illustrate why this is important, consider this alternative implementation of 'under2':
--
-- > under2 :: (Newtype n, Newtype n', o' ~ O n', o ~ O n)
-- >        => (o -> n) -> (n -> n -> n') -> (o -> o -> o')
-- > under2' pa f o0 o1 = ala pa (\p -> uncurry f . bimap p p) (o0, o1)
--
-- Being handed the "wraper", the /hof/ may apply it in any structure of its choosing –
-- in this case a tuple.
--
-- >>> ala Sum foldMap [1,2,3,4]
-- 10
ala :: (Newtype n, Newtype n', o' ~ O n', o ~ O n)
    => Wrapper o n -> (Wrapper o n -> b -> n') -> (b -> o')
ala pa hof = ala' pa hof id

-- | This is the original function seen in Conor McBride's work.
-- The way it differs from the 'ala' function in this wrapage,
-- is that it provides an extra hook into the \"wraper\" passed to the hof.
-- However, this normally ends up being @id@, so 'ala' wraps this function and
-- passes @id@ as the final parameter by default.
-- If you want the convenience of being able to hook right into the hof,
-- you may use this function.
--
-- >>> ala' Sum foldMap length ["hello", "world"]
-- 10
--
-- >>> ala' First foldMap (readMaybe @Int) ["x", "42", "1"]
-- Just 42
ala' :: (Newtype n, Newtype n', o' ~ O n', o ~ O n)
     => Wrapper o n -> ((a -> n) -> b -> n') -> (a -> o) -> (b -> o')
ala' _ hof f = unwrap . hof (wrap . f)

-- | A very simple operation involving running the function \'under\' the newtype.
--
-- >>> under Product (stimes 3) 3
-- 27
under :: (Newtype n, Newtype n', o' ~ O n', o ~ O n)
      => Wrapper o n -> (n -> n') -> (o -> o')
under _ f = unwrap . f . wrap

-- | The opposite of 'under'. I.e., take a function which works on the
-- underlying types, and switch it to a function that works on the newtypes.
--
-- >>> over All not (All False)
-- All {getAll = True}
over :: (Newtype n,  Newtype n', o' ~ O n', o ~ O n)
     => Wrapper o n -> (o -> o') -> (n -> n')
over _ f = wrap . f . unwrap

-- | Lower a binary function to operate on the underlying values.
--
-- >>> under2 Any (<>) True False
-- True
--
under2 :: (Newtype n, Newtype n', o' ~ O n', o ~ O n)
       => Wrapper o n -> (n -> n -> n') -> (o -> o -> o')
under2 _ f o0 o1 = unwrap $ f (wrap o0) (wrap o1)

-- | The opposite of 'under2'.
--
over2 :: (Newtype n, Newtype n', o' ~ O n', o ~ O n)
       => Wrapper o n -> (o -> o -> o') -> (n -> n -> n')
over2 _ f n0 n1 = wrap $ f (unwrap n0) (unwrap n1)

-- | 'under' lifted into a Functor.
underF :: (Newtype n, Newtype n', o' ~ O n', o ~ O n, Functor f, Functor g)
       => Wrapper o n -> (f n -> g n') -> (f o -> g o')
underF _ f = fmap unwrap . f . fmap wrap

-- | 'over' lifted into a Functor.
overF :: (Newtype n, Newtype n', o' ~ O n', o ~ O n, Functor f, Functor g)
      => Wrapper o n -> (f o -> g o') -> (f n -> g n')
overF _ f = fmap wrap . f . fmap unwrap

-- Instances from Control.Applicative

instance Newtype (WrappedMonad m a) where
  type O (WrappedMonad m a) = m a
  wrap = WrapMonad
  unwrap (WrapMonad a) = a

instance Newtype (WrappedArrow a b c) where
  type O (WrappedArrow a b c) = a b c
  wrap = WrapArrow
  unwrap (WrapArrow a) = a

instance Newtype (ZipList a) where
  type O (ZipList a) = [a]
  wrap = ZipList
  unwrap (ZipList a) = a

-- Instances from Control.Arrow

instance Newtype (Kleisli m a b) where
  type O (Kleisli m a b) = a -> m b
  wrap = Kleisli
  unwrap (Kleisli a) = a

instance Newtype (ArrowMonad a b) where
  type O (ArrowMonad a b) = a () b
  wrap = ArrowMonad
  unwrap (ArrowMonad a) = a

-- | @since 0.5.1
instance Newtype (Fixed a) where
  type O (Fixed a) = Integer
  wrap = MkFixed
  unwrap (MkFixed x) = x

-- Instances from Data.Functor.Compose

-- | @since 0.5.1
instance Newtype (Compose f g a) where
  type O (Compose f g a) = f (g a)
  wrap = Compose
  unwrap (Compose x) = x

-- Instances from Data.Functor.Const

instance Newtype (Const a x) where
  type O (Const a x) = a
  wrap = Const
  unwrap (Const a) = a

-- Instances from Data.Functor.Identity

instance Newtype (Identity a) where
  type O (Identity a) = a
  wrap = Identity
  unwrap (Identity a) = a

-- Instances from Data.Monoid

instance Newtype (Dual a) where
  type O (Dual a) = a
  wrap = Dual
  unwrap (Dual a) = a

instance Newtype (Endo a) where
  type O (Endo a) = (a -> a)
  wrap = Endo
  unwrap (Endo a) = a

instance Newtype All where
  type O All = Bool
  wrap = All
  unwrap (All x) = x

instance Newtype Any where
  type O Any = Bool
  wrap = Any
  unwrap (Any x) = x

instance Newtype (Sum a) where
  type O (Sum a) = a
  wrap = Sum
  unwrap (Sum a) = a

instance Newtype (Product a) where
  type O (Product a) = a
  wrap = Product
  unwrap (Product a) = a

instance Newtype (First a) where
  type O (First a) = Maybe a
  wrap = First
  unwrap (First a) = a

instance Newtype (Last a) where
  type O (Last a) = Maybe a
  wrap = Last
  unwrap (Last a) = a

instance Newtype (Alt f a) where
  type O (Alt f a) = f a
  wrap = Alt
  unwrap (Alt x) = x

-- Instances from Data.Ord

instance Newtype (Down a) where
  type O (Down a) = a
  wrap = Down
  unwrap (Down a) = a

instance Newtype (Min a) where
  type O (Min a) = a
  wrap = Min
  unwrap (Min a) = a

instance Newtype (Max a) where
  type O (Max a) = a
  wrap = Max
  unwrap (Max a) = a

instance Newtype (Data.Semigroup.First a) where
  type O (Data.Semigroup.First a) = a
  wrap = Data.Semigroup.First
  unwrap (Data.Semigroup.First a) = a

instance Newtype (Data.Semigroup.Last a) where
  type O (Data.Semigroup.Last a) = a
  wrap = Data.Semigroup.Last
  unwrap (Data.Semigroup.Last a) = a

instance Newtype (WrappedMonoid m) where
  type O (WrappedMonoid m) = m
  wrap = WrapMonoid
  unwrap (WrapMonoid m) = m

instance Newtype (Option a) where
  type O (Option a) = Maybe a
  wrap = Option
  unwrap (Option x) = x