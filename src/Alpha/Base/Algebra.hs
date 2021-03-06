-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE NoStarIsType #-}
module Alpha.Base.Algebra
(
    Arg, ArgMin, ArgMax,
    Bool(..), otherwise, 
    Monoid(..),
    Endo(..), All(..), Any(..),
    Semigroup(..), Min(..), Max(..), First(..), Last(..),
    Groupoid(..),
    Semigroupoid(..), 
    Ord,compare, 
    Ordering,
    Functor(..),
    Eq(..), (!=),
    Bounded(..), Enum(..),
)
where
import Data.Eq(Eq(..),(==),(/=))
import Data.Monoid(Monoid(mempty, mappend,mconcat))
import Data.Monoid(Endo(..), All(..), Any(..),  Alt(..))    
import Data.Bool(Bool(..), (&&), (||), not, otherwise)
import Data.Functor(Functor(..))
import Data.Groupoid(Groupoid(..))
import Data.Semigroup(Semigroup(..), Min(..), Max(..), First(..), Last(..),(<>), sconcat,Arg,ArgMin,ArgMax)
import Data.Ord(Ord(..),Ordering)    
import Data.Semigroupoid(Semigroupoid(..))
import Data.Foldable(Foldable, foldMap, fold, foldr, foldr', foldl, foldl')
import GHC.Enum(Bounded(..),Enum(..))
import Algebra.Lattice(Lattice(..))   




(!=)::(Eq a) => a -> a -> Bool
(!=) = (/=)
{-# INLINE (!=) #-}
infixl 4 !=    
