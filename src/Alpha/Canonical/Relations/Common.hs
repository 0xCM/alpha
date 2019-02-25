-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE PolyKinds #-}
module Alpha.Canonical.Relations.Common
(        
    module X,
    Relational(..),
    Relation(..),


) where
import Alpha.Base as X
import Alpha.Canonical.Common as X

-- | Specifies a relation between two elements    
newtype Relation a = Relation (a,a)    
    deriving (Eq,Ord,Data,Typeable)

-- Characterizes a binary relation on a set s    
class Eq a => Relational a where

    -- | Determines whether two elements are related
    related::P2 a
    related = (~?)
    {-# INLINE related #-}        

    -- | Infix synonym for 'related'
    (~?)::P2 a
    (~?) = related
    infixl 6 ~?

    relate::a -> a -> Relation a
    relate a b = Relation (a,b)

-- class Related a where
--     type Relation a 
--     type LeftPart a 
--     type RightPart a 

--     -- | Establishes a relation between two elements
--     relate::LeftPart a -> RightPart a -> Relation a 
    

