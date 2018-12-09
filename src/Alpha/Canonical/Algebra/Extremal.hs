module Alpha.Canonical.Algebra.Extremal
(
    
    Extremum(..),
    Infimum(..), Infimal(..),
    Supremum(..), Supremal(..),
    

) where
import Alpha.Base
import Alpha.Canonical.Relations
import Alpha.Canonical.Operators

type family Infimum a
type family Supremum a    
type family Extremum a


-- / Characterizes types for which a greatest lower bound can
-- be identified, with bounded intervals being the canonical
-- example
-- See https://en.wikipedia.org/wiki/Infimum_and_supremum    
class Infimal a where
    -- / The greatest lower bound
    infimum::a -> Extremum a
    

-- / Characterizes types for which a least upper bound can
-- be identified, with bounded intervals being the canonical
-- example
-- See https://en.wikipedia.org/wiki/Infimum_and_supremum    
class Supremal a where
    -- / The least upper bound
    supremum::a -> Extremum a
