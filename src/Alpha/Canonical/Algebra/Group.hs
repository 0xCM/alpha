-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Algebra.Group
(
    MultiplicativeGroup(..),
    AdditiveGroup(..),
    AbelianGroup(..),
    commutator,
    MonoidalAlt, MonoidalProduct, MonoidalSum,
    altM, sumM, prodM

) where
import Alpha.Base
import Alpha.Canonical.Algebra.Additive
import Alpha.Canonical.Algebra.Subtractive
import Alpha.Canonical.Algebra.Multiplicative
import Alpha.Canonical.Algebra.Reciprocative

import Alpha.Canonical.Relations
import Alpha.Canonical.Functions
import qualified Data.Monoid as Monoid

type MonoidalAlt f a = Monoid.Alt f a
type MonoidalSum a = Monoid.Sum a
type MonoidalProduct a = Monoid.Product a


class (Multiplicative a, Unital a, Reciprocative a) => MultiplicativeGroup a where

class (Additive a, Nullary a, Negatable a) => AdditiveGroup a where    

-- | A group for which the related commutator is always satisfied
class (AdditiveGroup a) => AbelianGroup a where

-- | Constructs a commutator for a binary operator
-- See https://en.wikipedia.org/wiki/Commutator
commutator::(Reciprocative a) => O2 a -> O2 a
commutator o =  \x y ->  o (o (reciprocal x) (reciprocal y)) (o x y) where

-- Lifts the input into the Alt monoid
-- Example:
-- alt Nothing  <> alt (Just 4) <> alt (Just 7)
-- >> Alt {getAlt = Just 4}
altM::Monoid a => f a -> MonoidalAlt f a
altM = Monoid.Alt

sumM::Monoid a => a -> MonoidalSum a
sumM = Monoid.Sum

prodM::Monoid a => a -> MonoidalProduct a
prodM = Monoid.Product



instance (Integral a) => MultiplicativeGroup (Ratio a) where
instance MultiplicativeGroup Float where 
instance MultiplicativeGroup Double where 
instance MultiplicativeGroup CFloat where 
instance MultiplicativeGroup CDouble where 

instance AdditiveGroup Integer where 
instance AdditiveGroup Int where 
instance AdditiveGroup Int8 where 
instance AdditiveGroup Int16 where 
instance AdditiveGroup Int32 where 
instance AdditiveGroup Int64 where     
instance (Integral a) => AdditiveGroup (Ratio a) where 
instance AdditiveGroup Float where 
instance AdditiveGroup Double where 
instance AdditiveGroup CFloat where 
instance AdditiveGroup CDouble where 
    
instance AbelianGroup Integer where 
instance AbelianGroup Int where 
instance AbelianGroup Int8 where 
instance AbelianGroup Int16 where 
instance AbelianGroup Int32 where 
instance AbelianGroup Int64 where     
instance (Integral a) => AbelianGroup (Ratio a) where 
instance AbelianGroup Float where 
instance AbelianGroup Double where 
instance AbelianGroup CFloat where 
instance AbelianGroup CDouble where 
        