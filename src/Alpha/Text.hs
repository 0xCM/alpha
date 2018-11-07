-----------------------------------------------------------------------------
-- | Defines the Text segment API surface
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE NoImplicitPrelude #-}

module Alpha.Text
(   
    module Alpha.Text.Text,
    module Alpha.Text.Json,
    module Alpha.Text.Format,
    module Alpha.Text.Combinators,
    module Alpha.Text.String,

    module Text.Show,
    T.Text
    
) where
import Text.Show

import qualified Data.Text as T
import qualified Data.String as S

import Alpha.Text.Json
import Alpha.Text.Format
import Alpha.Text.Combinators
import Alpha.Text.String
import Alpha.Text.Text
import Alpha.Canonical




