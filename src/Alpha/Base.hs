{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}

module Alpha.Base
(
    module DB,
    module GB,
    module Text.Show,
    Float, Float#, 
    Double, Double#,
    ByteString.ByteString,    
    IO,
    div,
    flip,
    (++)

    
) where

import System.IO(IO)
import Prelude(map, length, zip, undefined, flip, fromIntegral,div)
import qualified Data.Text as Text
import qualified Data.ByteString as ByteString
import GHC.Float(Float, Float#, Double, Double#)
import Text.Show
import Data.List as List
import Alpha.Data.Base as DB
import Alpha.GHC.Base as GB

