-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Algebra.Module
(
    LeftModule(..), 
    RightModule(..),
        
) where
import Alpha.Canonical.Relations
import Alpha.Canonical.Algebra.Action
import Alpha.Canonical.Algebra.Group
import Alpha.Canonical.Algebra.Ring
import Alpha.Canonical.Algebra.Additive
import Alpha.Canonical.Algebra.Multiplicative

-- Note that the category of left modules over a ring R is isomorphic to
-- the category or right modules over the opposite ring R^op

-- | A left module over a ring r
-- See Y2018MTLA,
class (Ring r, AbelianGroup m, LeftAction r m) 
    => LeftModule r m where
    
-- | A right module over a ring r
class (Ring r, AbelianGroup m, RightAction m r) 
    => RightModule m r where
    
