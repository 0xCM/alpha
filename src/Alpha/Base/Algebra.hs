{-# LANGUAGE NoStarIsType #-}
-----------------------------------------------------------------------------
-- | Base Data.* modules
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Base.Algebra
(
    Arg, ArgMin, ArgMax,
    Bool(..),(&&), (||), not, otherwise,
    Monoid(..),
    Dual(..), Endo(..), All(..), Any(..),
    Semigroup(..), Min(..), Max(..), First(..), Last(..),
    Groupoid(..),
    Semigroupoid(..), 
    TotalOrder, Ord, Ordering,
    Functor(..),
    Eq(..), (!=),
    Bounded(..), Enum(..)

)
where
import Data.Eq(Eq(..),(==),(/=))
import Data.Monoid(Monoid(mempty, mappend,mconcat))
import Data.Monoid(Dual(..), Endo(..), All(..), Any(..),  Alt(..))    
import Data.Bool(Bool(..), (&&), (||), not, otherwise)
import Data.Functor(Functor(..))
import Data.Groupoid(Groupoid(..))
import Data.Semigroup(Semigroup(..), Min(..), Max(..), First(..), Last(..),(<>), sconcat,Arg,ArgMin,ArgMax)
import Data.Ord(Ord,Ordering)    
import Data.Semigroupoid(Semigroupoid(..))
import Data.Foldable(Foldable, foldMap, fold, foldr, foldr', foldl, foldl')
import GHC.Enum(Bounded(..),Enum(..))

type TotalOrder a = Ord a


(!=)::(Eq a) => a -> a -> Bool
(!=) = (/=)
{-# INLINE (!=) #-}
infixl 4 !=    
