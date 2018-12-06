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
    module FB,
    module CB,
    module TB,
    IO,
    Storable,
    div,
    (++)

    
) where

import GHC.Float(Float, Float#, Double, Double#)
import Data.List as List
import Alpha.Data.Base as DB
import Alpha.Functor.Base as FB
import Alpha.Control.Base as CB
import Foreign.Storable(Storable)
import Alpha.Text.Base as TB
import System.IO(IO)
import Prelude(div)
