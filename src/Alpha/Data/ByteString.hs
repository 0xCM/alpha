-----------------------------------------------------------------------------
-- | ByteString-related utilities
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
module Alpha.Data.ByteString
(
    bytes,
    segment, segments,
    bytestring
)
where


import qualified Data.ByteString as EG
import qualified Data.ByteString.Lazy as LZ

import Alpha.Canonical
import Data.Word
import Data.Int
import Data.Functor
import Data.Function
import Data.List(takeWhile,iterate)
import Data.Bool
import System.Entropy

import qualified Data.Text as Text
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LC8    
import qualified Data.ByteString.Char8 as C8

import GHC.Enum
import GHC.Real
import Data.Eq
import Data.Ix(range)
import GHC.Exts
import qualified Data.Ix as Ix
import qualified Data.List as L
import Alpha.Canonical
import Alpha.Data.Numbers
import Alpha.Text.Combinators
import Alpha.Math.Arithmetic
import Data.Tuple

type instance Eager EG.ByteString = EG.ByteString

type instance Lazy EG.ByteString = LZ.ByteString



-- Extracts a contiguous sequence of bytes from the source
-- of length w starting at the 0-based index i
bytes::(Indexed a Word8) => Int -> Int -> a -> [Word8]
bytes i width src = fmap (\k -> src ! k) [i..(i+width)]

segment::(Int,Int) -> EG.ByteString -> EG.ByteString
segment (m, n) bs = EG.splitAt m bs |> snd |> EG.splitAt (n - m - 1) |> fst

-- Constructs a bytestring from a list of words
bytestring::[Word8] -> EG.ByteString
bytestring = EG.pack

-- Partitions a bytestring into segments of a specified length
chunk :: Int -> EG.ByteString -> [EG.ByteString]
chunk n = takeWhile (not . EG.null) . fmap (EG.take n) . iterate (EG.drop n)

entropy::Int -> EG.ByteString
entropy n = (getHardwareEntropy n |> shredIO ) |> fromJust 

segments::Int->Int->[(Int,Int)]
segments width total = intervals    
    where 
        source = entropy total
        len = length source
        cutpoints = range(1, len-1)
                  |>fmap (\i ->  case divides i width of
                                    True -> i
                                    False -> 0)
                  |> L.filter (\i -> i /= 0)
        intervals = cutpoints |> fmap (\c -> (c - width, c - 1))
        segs = intervals |> fmap (\x -> segment x source)


-- 'ByteString' -> 'Text'
instance Formattable EG.ByteString where
    --format x = C8.unpack x |> Text.pack 
    format x = (bytes 0 (EG.length x) x) |> format

-- 'LBS.ByteString' -> 'Text'    
instance Formattable LZ.ByteString where
    format  = Text.pack . LC8.unpack 

instance ToLines EG.ByteString where    
    lines bs = C8.lines bs |> fmap format

instance Packable [Word8] EG.ByteString where
    pack = EG.pack
    unpack = EG.unpack 

instance Concatenable EG.ByteString EG.ByteString where
    type Concatenated EG.ByteString EG.ByteString = EG.ByteString
    concat = EG.append

instance Length EG.ByteString where
    length = convert . EG.length 

instance Indexed EG.ByteString Word8 where    
    item = EG.index

instance Packable [Word8] LZ.ByteString where
    pack = LZ.pack
    unpack = LZ.unpack 
    
instance Concatenable LZ.ByteString LZ.ByteString where
    type Concatenated LZ.ByteString LZ.ByteString = LZ.ByteString
    concat = LZ.append
    
instance Length LZ.ByteString where
    length = convert . LZ.length 

instance Indexed LZ.ByteString Word8 where    
    item x n = LZ.index x (int64 n)

instance Convertible EG.ByteString [Word8] where
    convert source = source |> bytes 0 (length source - 1) 

