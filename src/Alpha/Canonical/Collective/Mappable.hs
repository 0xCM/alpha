-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Collective.Mappable
(
    Mappable(..),
    mapi,
    

) where
import Alpha.Base
import Alpha.Canonical.Algebra.Subtractive

import qualified Data.List as List

class Mappable c a b where    
    type Mapped c a b
    map::(a -> b) -> c -> Mapped c a b

mapi::(Integral i, Subtractive i) => ((i,a) -> b) -> [a] -> [b]
mapi f l = f <$> z where 
    idx = [0..upper]
    upper  = (fromIntegral $ List.length l) - 1
    z = List.zip idx l

instance Mappable [a] a b where
    type Mapped [a] a b = [b]
    map = List.map
    
instance Mappable (Seq a) a b where
    type Mapped (Seq a) a b = Seq b
    map = fmap
    
    
            