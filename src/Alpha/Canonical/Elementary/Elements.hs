-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE UndecidableInstances #-}

module Alpha.Canonical.Elementary.Elements
(
    Structure(..),    
    Finite(..),
    Vectored(..),
    Individual(..)    
)
where
import Alpha.Base

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Sequence as Sequence

type family Individual a
type instance Individual [a] = a

    
-- | Classifies a type that structures a set of individuals according
-- to axioms of the particular structure. This is in contradistinction
-- to a 'Set' which is merely a collection of elements. Note also that
-- a structure parametrized by a does not necessarily lead to a collection
-- of elements of type 'a'. This is true only in the degenerate case - which
-- also happens to be the default
class Structural a where
    structure::a -> Structure a

data family Structure a    


-- | Characterizes a type inhabited by a finite set of
-- values and for which a count is determined
class Finite a where
    -- | Counts the number of items within the purview of the subject
    count::(Integral n) => a -> n
    
class Vectored s a where
    vector::s -> Vector a
    
