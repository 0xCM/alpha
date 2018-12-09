module Alpha.Canonical.Algebra.Module
(
    LeftModule(..), 
    RightModule(..),
        
) where
import Alpha.Base 
import Alpha.Canonical.Operators
import Alpha.Canonical.Algebra.Scaled
import Alpha.Canonical.Algebra.Group
import Alpha.Canonical.Algebra.Ring

-- | A left module over a ring r
class (Ring r, AbelianGroup g, LeftScalar r g) 
    => LeftModule r g where
    
-- | A right module over a ring r
class (Ring r, AbelianGroup g, RightScalar g r) 
    => RightModule g r where
