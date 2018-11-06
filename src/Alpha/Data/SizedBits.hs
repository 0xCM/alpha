module Alpha.Data.SizedBits where

import qualified Data.Text as Text
import Data.Bits
import Alpha.Base
import Alpha.Canonical
import Alpha.Text.Combinators
import Alpha.Data.Numbers
import Alpha.Data.Bit
--import Alpha.Data.Bits
import Alpha.Data.Numbers
import Alpha.Data.Natural
import GHC.TypeLits



class (KnownNat n, FiniteBits a) => SizedBits n a where
    width::Int
    width = natval @n 
    

-- wordsplat::forall n a. (KnownNat n, SizedBits n a) => SizedWord n -> SizedWord n -> SizedWord (n + n)
-- wordsplat x y = left .|. right
--         where 
--             size = natVal @n
--             left = (fromIntegral x) .<<. (fromIntegral size)
--             right = fromIntegral y 

-- (.<<.) :: SizedBits n a => a -> Int -> a
-- (.<<.) n i = shiftL n i
-- infixl 8 .<<.
                        