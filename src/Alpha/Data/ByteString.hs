-----------------------------------------------------------------------------
-- | ByteString-related utilities
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
module Alpha.Data.ByteString
(
    bytes,
    segment, 
    segments,
    bytestring
)
where
import System.IO.Unsafe
import System.Entropy
import Data.Word
import Data.Int
import Data.Functor
import Data.Function
import Data.Bool
import Data.Tuple
import Data.Eq

import Data.List(takeWhile,head)

import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LC8    
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString as EG
import qualified Data.ByteString.Lazy as LZ

import Data.Ix(range)
import GHC.Enum
import GHC.Real
import GHC.Exts
import GHC.TypeLits(natVal, KnownNat)
import qualified Data.Ix as Ix
import qualified Data.List as L

import Alpha.Data.Conversion
import Alpha.Text.Combinators
import Alpha.Canonical hiding(range)
import Alpha.Data.Maybe
import Alpha.Data.Bits

type instance Concatenated EG.ByteString EG.ByteString = EG.ByteString
type instance Concatenated LZ.ByteString LZ.ByteString = LZ.ByteString
type instance Element LZ.ByteString = Word8
type instance Element EG.ByteString = Word8

-- Extracts a contiguous sequence of bytes from the source
-- of length w starting at the 0-based index i
bytes::(Indexed a Int) => Int -> Int -> a -> [Element a]
bytes i width src = fmap (\k -> src ! k) [i..(i + width)]

segment::(Int,Int) -> EG.ByteString -> EG.ByteString
segment (m, n) bs = EG.splitAt m bs |> snd |> EG.splitAt (n - m - 1) |> fst

-- Constructs a bytestring from a list of words
bytestring::[Word8] -> EG.ByteString
bytestring = EG.pack
    
entropy::Int -> EG.ByteString
entropy n = (getHardwareEntropy n |> unsafeDupablePerformIO ) |> fromJust

segments::Int->Int->[(Int,Int)]
segments width total = intervals    
    where 
        source = entropy total
        len = length source
        cutpoints = range(1, len - 1)
                  |>fmap (\i ->  case divides i width of
                                    True -> i
                                    False -> 0)
                  |> L.filter (\i -> i /= 0)
        intervals = cutpoints |> fmap (\c -> (c - width, c - 1))
        segs = intervals |> fmap (\x -> segment x source)

instance Formattable EG.ByteString where
    format x = (bytes 0 (EG.length x) x) |> format

instance Formattable LZ.ByteString where
    format  = Text.pack . LC8.unpack 

instance ToLines EG.ByteString where    
    lines bs = C8.lines bs |> fmap format

instance IsList EG.ByteString where
    type Item (EG.ByteString) = Word8
    toList = EG.unpack
    fromList = EG.pack

instance IsList LZ.ByteString where
    type Item (LZ.ByteString) = Word8
    toList = LZ.unpack
    fromList = LZ.pack
    
instance Container EG.ByteString

instance Container LZ.ByteString

instance Packable [Word8] EG.ByteString where
    pack = EG.pack
    unpack = EG.unpack 
        
instance Appendable EG.ByteString EG.ByteString where    
    append = EG.append

instance Length EG.ByteString where
    length = convert . EG.length 

instance Indexed EG.ByteString Int where 
    lookup = EG.index

instance Packable [Word8] LZ.ByteString where
    pack = LZ.pack
    unpack = LZ.unpack 
    
instance Appendable LZ.ByteString LZ.ByteString where
    append = LZ.append
    
instance Length LZ.ByteString where
    length = convert . LZ.length 

instance Indexed LZ.ByteString Int where    
    lookup x n = LZ.index x (int64 n)

    
instance Convertible EG.ByteString [Word8] where
    convert source = source |> bytes 0 (length source - 1) 

instance Chunkable EG.ByteString where
    chunk n = takeWhile (not . EG.null) . fmap (EG.take n) . iterate (EG.drop n)    
        