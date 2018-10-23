-----------------------------------------------------------------------------
-- | The Concatenable concept
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}
module Alpha.Data.Concatenable where

import qualified Data.List as List
import Data.List((++))
import qualified Data.Text as Text
import Alpha.Data.Base
import Alpha.Canonical
    
instance Concatenable [a] [a] where    
    type Concatenated [a] [a] = [a]
    concat x y = x ++ y

instance Concatenable Text Text where
    type Concatenated Text Text = Text
    concat  = Text.append
        
instance Concatenable Text Char where
    type Concatenated Text Char = Text
    concat t c  = Text.pack  [c] |> Text.append t 
    
instance Concatenable Char Text where
    type Concatenated Char Text = Text
    concat c t  = Text.append (Text.pack [c]) t

instance Concatenable Char Char where
    type Concatenated Char Char = Text
    concat c1 c2  = Text.pack ([c1] ++ [c2])
        