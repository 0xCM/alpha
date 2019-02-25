-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Common.FiniteList
(
    FiniteList(..)
)
where
import Alpha.Canonical.Common.Root
import Alpha.Canonical.Common.Individual
import Alpha.Canonical.Common.Indexing
import Alpha.Canonical.Common.Sequential
import Alpha.Canonical.Common.Setwise
import Alpha.Canonical.Common.Concat

newtype FiniteList a = FiniteList [a]
    deriving (Eq, Generic, Data, Typeable, 
        Functor, Foldable, Traversable, Applicative, Monad, Semigroup, Apply,
        Nullity, Existential, Universal, FinitelyCountable,
        HasFirst, HasLast, Endpointed, Show, Groupable, Concatenable,
        Singleton, Reversible, Listed, Set, Setwise, Paged) 
    deriving (Formattable) via ([a])
instance Newtype(FiniteList a)

type instance Collapsed [FiniteList a] = FiniteList a
type instance Individual (FiniteList a) = a

-------------------------------------------------------------------------------        
-- * FiniteList member instances
-------------------------------------------------------------------------------    

instance (Eq a) => Indexable (FiniteList a) where
    (FiniteList elements) !! i = elements !! i

instance Collapsible [FiniteList a] where
    collapse lists = unwrap <$> lists |> collapse |> wrap

instance IsList (FiniteList a) where
    type Item (FiniteList a) = a
    fromList = FiniteList
    toList (FiniteList list) = list
    