module Alpha.Text.Char where

import Alpha.Canonical
import Data.Char(Char)
import qualified Data.Text as T

instance Length Char where
    length c = 1

-- | 'Char' -> 'String'    
instance ToString Char where
    string x = [x]

-- | 'Char' -> 'Text' 
instance Formattable Char where
    format x = T.singleton x
