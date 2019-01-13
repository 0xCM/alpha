-----------------------------------------------------------------------------
-- | 
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Canonical.Common.Length
(
    Length(..),
    
) where
import Alpha.Canonical.Common.Root
import Alpha.Canonical.Common.Conversions
import qualified Data.Text as Text
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.ByteString as EG
import qualified Data.ByteString.Lazy as LZ
import qualified Data.List.NonEmpty as NonEmpty


class Length a where    
    length::(Integral n) => a -> n

instance Length [a] where
    length =  integral . List.length

instance Length Text where
    length =  integral . Text.length    

instance Length Char where
    length _ = 1

instance Ord a => Length (Set' a) where
    length  =  integral . Set.size  
    
instance Length (NonEmpty a) where
    length = fromIntegral . NonEmpty.length
    