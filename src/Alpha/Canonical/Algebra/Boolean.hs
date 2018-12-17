module Alpha.Canonical.Algebra.Boolean
(
    Boolean(..),    
    

) where
import Alpha.Base
import Alpha.Native

-- | Characterizes a value that can be converted to a 'Bool'
class Boolean a where
    bool::a -> Bool    

    