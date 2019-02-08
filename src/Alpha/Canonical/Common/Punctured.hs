-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Common.Punctured
(
    Punctured(..),
    Puncture(..),
    NonzeroDouble(..),
    NonzeroFloat(..),
    NonzeroRatio(..)
)
where
import Alpha.Canonical.Common.Root


-- | Represents a type for which a single value has been deleted
newtype Punctured p a = Punctured a
    deriving (Eq, Functor, Foldable, Traversable, Generic, Data, Typeable) 
instance Newtype (Punctured p a)

class Puncture a b where
    deleted::b    


type NonzeroRatio a = Punctured 0 (Ratio a)
type NonzeroDouble = Punctured 0 Double
type NonzeroFloat = Punctured 0 Float

instance Integral a => Puncture (NonzeroRatio a) a where
    deleted = 0
instance Puncture NonzeroFloat Float where
    deleted = 0
instance Puncture NonzeroFloat Double where
    deleted = 0
            
    
    
