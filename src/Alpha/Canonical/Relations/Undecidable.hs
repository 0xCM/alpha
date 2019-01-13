-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE UndecidableInstances #-}
module Alpha.Canonical.Relations.Undecidable where
import Alpha.Base
import Alpha.Canonical.Relations.Related

import qualified Prelude as P

-- | Encodes that reflexive and tansitive relations are, by definition, preorders
-- instance (Reflexive a, Transitive a) => Preorder a where
--     preorder x y = (x,y)

-- | Encodes that reflexive and tansitive relations are, by definition, equivalence relations
-- instance (Reflexive a, Symmetric a, Transitive a) => Equivalence a where
--     eqivilate x y = (x,y)


