module Alpha.Text.String where

import Data.String(String)
import Data.Function
import qualified Data.String as S
import qualified Data.Text as T
import qualified Data.List as List
import Alpha.Canonical
import Alpha.Data.Numbers


instance Formattable String where
    format = T.pack 

instance Length String where
    length = convert . List.length
    

