-----------------------------------------------------------------------------
-- | 0 1
-- Copyright   :  (c) Chris Moore, 2018 + Contributors per license
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}

module Alpha.Data.BitString
(
    bitstring

) where
import Alpha.Canonical
import Alpha.Data.Bit
import qualified Data.List as List

-- | Encapsulates an ordered sequence of bits
newtype BitString = BitString [Bit]   
    deriving (Eq, Ord, Generic, Data, Typeable, Read)
instance Newtype (BitString)

-- | Constrcts a 'Bitstring' value from an integer
bitstring::(IntegralDomain a, Integral a) => a -> BitString
bitstring i = BitString $ bitstring' (quotRem i 2) []  where
    bitstring' (n,d) r = seq c $
        case n of
        0 -> r'
        _ -> bitstring' (quotRem n 2) r' 
        where
            c  = ifelse (d == 0) off on
            r' = c : r            

instance Formattable BitString where
    format (BitString bits) =  format <$> bits |> append |> prefix n
        where n =  length bits |> format |> parenthetical |> spaced

instance ToInteger BitString where
    integer = undefined

instance Show BitString where
    show = string . format
            
