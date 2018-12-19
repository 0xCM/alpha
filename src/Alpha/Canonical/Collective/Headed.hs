-----------------------------------------------------------------------------
-- | Structures over which sequential operations may be defined
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Collective.Headed
(
    Headed(..)

) where

import Alpha.Base
import Alpha.Canonical.Functions
import Alpha.Canonical.Elementary

import qualified Data.List as List
import qualified Data.Sequence as Seq
import qualified Data.Stream.Infinite as Stream

-- | Classifies a structure that can be partitioned into two sets:
-- A singleton set containing the "first" element and another set containing
-- the remainder
class Headed a where
    -- | Retrives the first item in the sequence
    head::a -> Element a

        -- | Skips the first item of the sequence and returns the remainder
    tail::a -> a

instance (Eq a) => Headed [a] where
    head = List.head
    tail = List.tail
    
instance (Eq a) => Headed (Seq a) where    
    head s = Seq.index s 0
    tail s = snd $ Seq.splitAt 1 s
    
instance Headed (Stream a) where        
    head s = s Stream.!! 0
    tail = Stream.tail
    