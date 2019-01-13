-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE PolyKinds #-}

module Alpha.Canonical.Structures.Common
(
    module X,
    Space(..),
    EndSet(..),
    HomSet(..),
    ObjSet(..)
) where
import Alpha.Canonical.Algebra as X

data family Space (a::Constraint)    

-- Represents families of endomorphisms within the same category 
data family EndSet (c::Constraint) (x::Constraint) (y::Constraint)

-- Represents families of morphisms from a category c to a category d
data family HomSet (c::Constraint) (x::Constraint) (d::Constraint) (y::Constraint)

-- | Represents families of (categorical) objects
data family ObjSet (c::Constraint)

