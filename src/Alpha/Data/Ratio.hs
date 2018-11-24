module Alpha.Data.Ratio
(
    Ratio, numerator, denominator, ratio, (%)
)
where

import Alpha.Base
import Data.Ratio(numerator,denominator)
import qualified GHC.Real as GR
import qualified Data.Ratio as DR
import Alpha.Data.Asci

-- Newtype abstraction over base-defined ratio
newtype Ratio n = Ratio (DR.Ratio n)
    deriving (Enum, Eq, Num, Ord, Real,Storable)

instance (Show n) => Show (Ratio n) where
    show (Ratio r) = ( show $ numerator r) ++ FSlash ++ (show $ denominator r) where
                
-- Forms the 'Ratio' of two integral values        
ratio::(Integral n) => n -> n -> Ratio n
ratio m n =  Ratio $  (GR.%) m n

-- Infix operator synonym for 'mod' function
(%)::(Integral n) => n -> n -> n
(%) = GR.mod

infixl 5 %