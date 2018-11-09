-----------------------------------------------------------------------------
-- | Reflection-related Utilities 
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Alpha.Data.Reflection where

import Data.Data(Data(..),DataType)  
import Data.Typeable(TypeRep,Typeable,typeOf)  
import Data.Default(Default(..))

-- | Retrieves the 'DataType' metadata for the type 'a'
datatype:: forall a. (Data a, Default a) => DataType
datatype =   dataTypeOf (def @a)

-- | Retrieves the 'TypeRep' metadata for the type 'a'
typeof:: forall a. (Typeable a, Default a) => TypeRep
typeof = typeOf (def @a)
