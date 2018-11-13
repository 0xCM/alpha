{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Alpha
( 
    module X,
    module A,
    module D,
    module TL,
    module S,
    module IO,
    module Text,
    Coercible(..), Coercion(..)
  
) where
    
import Alpha.Base as X hiding((<=),(>=),(<),(>),zero)

import Alpha.Canonical as A
import Alpha.Text as Text
import Alpha.IO as IO
import Alpha.TypeLevel as TL
import Alpha.Data as D
import Alpha.System as S

import Data.Coerce(Coercible(..))
import Data.Type.Coercion



