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
    module CB,
    module DF,
    IO,
    div,
    (++),
    

    
) where

--import GHC.Float(Float, Float#, Double, Double#)
import Data.List as List
import Alpha.Data.Base as DB hiding( (/))
import Alpha.Data.Functor as DF
import Alpha.Control.Base as CB
import Foreign.Storable(Storable)
import System.IO(IO)
import Prelude(div)
import qualified GHC.Base as GB
