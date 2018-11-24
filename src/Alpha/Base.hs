-----------------------------------------------------------------------------
-- | Defines the common API service for accessing base and 3rd-party libs
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MagicHash #-}
module Alpha.Base
(
    module DB,
    module GB,
    module FB,
    module CB,
    module Text.Show,
    Float, Float#, 
    Double, Double#,
    ByteString.ByteString,    
    IO,
    Set,
    Storable,
    Coercible(..), Coercion(..),
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
import Alpha.Functor.Base as FB
import Alpha.Control.Base as CB
import Data.Set(Set)
import Foreign.Storable(Storable)
import Data.Coerce(Coercible(..))
import Data.Type.Coercion
