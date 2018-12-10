-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Collective.Indexed
(
    Indexed(..),
    IndexedChoice(..),
    splitAt, 
) where
import Alpha.Base
import qualified Data.List as List
import qualified Data.Vector as Vector
import qualified Data.Sequence as Sequence
import Alpha.Canonical.Element

-- | Characterizes a structure of type s holding elements indexed by a value of type i
class Indexed s i where

    lookup::s -> i -> Element s

    (!)::s -> i -> Element s
    (!) = lookup            
    infixr 0 !

-- Characterizes a type that represents a finite sequence of mutually disjoint choices
class IndexedChoice a where    
    choiceix::a -> Int

-- | Makes the 'List.genericSplitAt' function the 'default' splitAt function
splitAt::(Integral i) => i -> [a] -> ([a],[a])
splitAt = List.genericSplitAt

instance (Eq a) => Indexed [a] Int where    
    lookup = (List.!!)
    
instance Indexed (Seq a) Int where
    lookup = Sequence.index
    
instance Indexed (Vector a) Int where
    lookup = (Vector.!)
    