-----------------------------------------------------------------------------
-- | Utilties to facilitate record manipulation
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}

module Alpha.Data.Records(field) where

import Data.Tagged
import Data.Kind
import Data.Function
import Data.Int
import Data.Maybe
import Data.Text

r :: (r -> a) -> r -> a
r f = f

-- | Extracts a field from a record
field :: r -> (r -> f) -> f 
field = flip ($)
  
-- type Person1 = Record
--     '[ Tagged "id" Int32
--      , Tagged "name" Text
--      , Tagged "age" (Maybe Int32)
--     ]
  
data Person2 = Person2 {
    name :: Text,
    address :: Text
}

data Business = Business {
    name :: Text,
    address :: Text
}

makePerson::Person2
makePerson = Person2 {
    name = "Hank",
    address = "Acme Rd."
}

makeBusiness::Business
makeBusiness = Business {
    name = "Acme",
    address = "Hank Rd"
}

getName::(Text,Text)
getName = (pname, bname)
    where 
        person = makePerson
        pname =  field @Person2 person name
        biz = makeBusiness
        bname = r @Business name biz