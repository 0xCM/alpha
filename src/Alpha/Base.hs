{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}

module Alpha.Base
(
    module X,
    module Text.Show,
    Float, Float#, 
    Double, Double#,
    ByteString.ByteString,    
    IO,
    undefined,
    div,
    flip,
    (++)

    
) where

import System.IO
import Prelude(map, length, zip, undefined, flip, fromIntegral,div)
import qualified Data.Text as Text
import qualified Data.ByteString as ByteString
import GHC.Float
import Text.Show
import Data.List as List
import Alpha.Data.Base as X
import Alpha.GHC.Base as X

