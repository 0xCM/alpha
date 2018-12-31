-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Elementary.Individual
(
    Individual(..),
    Membership(..),

    Tuple(..), UniTuple(..), 
    Tuple1(..), UniTuple1(..), 

) where
import Alpha.Canonical.Common

type family Individual a
type instance Individual [a] = a
type instance Individual Integer = Integer
type instance Individual Int = Int
type instance Individual Int8 = Int8
type instance Individual Int16 = Int16
type instance Individual Int32 = Int32
type instance Individual Int64 = Int64
type instance Individual (Ratio a) = Ratio a
type instance Individual Natural = Natural
type instance Individual Word = Word
type instance Individual Word8 = Word8
type instance Individual Word16 = Word16
type instance Individual Word32 = Word32
type instance Individual Word64 = Word64

newtype Tuple1 a1 = Tuple1 a1
    deriving (Eq,Ord)

type instance Individual (UniTuple1 a) = a
type instance Individual (UniTuple2 a) = a
type instance Individual (UniTuple3 a) = a
type instance Individual (UniTuple4 a) = a
type instance Individual (UniTuple5 a) = a


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

class Membership a where
    members::a -> [Individual a]    
