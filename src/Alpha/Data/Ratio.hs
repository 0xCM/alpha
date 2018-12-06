module Alpha.Data.Ratio
(
    Ratio,numerator,denominator, ratio
)
where

import Alpha.Base
import Data.Ratio(numerator,denominator)
import Alpha.Text.Asci
import qualified Data.Ratio as DR

-- Newtype abstraction over base-defined ratio
newtype Ratio n = Ratio (DR.Ratio n)
    deriving (Enum, Eq, Num, Ord, Real,Storable)

instance (Show n) => Show (Ratio n) where
    show (Ratio r) = ( show $ numerator r) ++ FSlash ++ (show $ denominator r) where
                
-- Forms the 'Ratio' of two integral values        
ratio::(Integral n) => n -> n -> Ratio n
ratio m n =  Ratio $  (DR.%) m n

