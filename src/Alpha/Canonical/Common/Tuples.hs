-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Common.Tuples
(
    Tuple1(..), Tuple(..),
    UniTuple1(..), UniTuple(..)

) where
import Alpha.Canonical.Common.Root
import Alpha.Canonical.Common.Individual
import Alpha.Canonical.Common.Synonyms

newtype Tuple1 a1 = Tuple1 a1
    deriving (Eq,Ord)

newtype UniTuple1 a = UniTuple1 a
    deriving (Eq,Ord)

-- Unifies tuple types (for supported arities)
type family Tuple (n::Nat) a = r | r -> a where
    Tuple 1 a1 = Tuple1 a1
    Tuple 2 (a1,a2) = (a1,a2)
    Tuple 3 (a1,a2,a3) = (a1,a2,a3)
    Tuple 4 (a1,a2,a3,a4) = (a1,a2,a3,a4)
    Tuple 5 (a1,a2,a3,a4,a5) = (a1,a2,a3,a4,a5)

type family UniTuple (n::Nat) a = r | r -> a where
    UniTuple 1 a = UniTuple1 a
    UniTuple 2 a = UniTuple2 a
    UniTuple 3 a = UniTuple3 a
    UniTuple 4 a = UniTuple4 a
    UniTuple 5 a = UniTuple5 a


type instance Individual (UniTuple1 a) = a
type instance Individual (UniTuple2 a) = a
type instance Individual (UniTuple3 a) = a
type instance Individual (UniTuple4 a) = a
type instance Individual (UniTuple5 a) = a
