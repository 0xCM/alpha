-----------------------------------------------------------------------------
-- | Defines the JSON API surface
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Alpha.Text.Json

where
    
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Aeson.Encode.Pretty
import Data.Functor
import Alpha.Base
import Alpha.Canonical
import Alpha.Text.Format
import Alpha.Data.ByteString
        

-- | Converts json to formatted text    
json :: ToJSON a => a -> Text
json value = value |> encodePretty |> format
    