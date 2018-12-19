-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Elementary.Discrete
(
    Discrete(..), Aggregate(..),
    Woven(..), Weavable(..),
    Assembly(..),    
) where

import Alpha.Base


import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Stream.Infinite as Stream

type family Woven g t

type instance Woven a [a] = [a]
type instance Woven g (Stream g) = Stream g
type instance Woven Char Text = Text

-- Characterizes a discretizable structure that can be converted to/from
-- a canonical linear ordering of its elements
class Discrete a where
    type Individual a   
    
    -- Partition a discretizable structure into its canonical linear order
    points::a -> [Individual a]
        
class (Discrete a) =>  Aggregate a where

    -- | Consolidate a collection of individuals of a discrete sctructure; the converse of 'discretize'
    consolidate::[Individual a] -> a
        
class Weavable g t where
    -- Weaves a grain 'g' with a target 't' to produce a 'Woven g t' value
    weave::g -> t -> Woven g t        
    
-- | Characterizes a structure that supports invertible construction/destruction operations
class Assembly a b | a -> b, b -> a where
    disassemble::a -> [b]    
    assemble::[b] -> a

instance Discrete [a] where
    type Individual [a] = a
    points = id    

instance Discrete Text where
    type Individual Text = Char
    points = Text.unpack
        
instance Weavable Char Text where
    weave = Text.intersperse
            
instance Weavable g [g] where
    weave = List.intersperse

instance Weavable g (Stream g) where
    weave = Stream.intersperse
    
instance Assembly [a] a where
    assemble = id
    disassemble = id
