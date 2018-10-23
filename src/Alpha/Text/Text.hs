module Alpha.Text.Text where

import Text.Show
import qualified Data.Text as T
import qualified Data.String as S
import Alpha.Canonical
import Alpha.Data.Numbers

instance Length T.Text where
    length t =   convert (T.length t)
    
instance Packable S.String T.Text where
    pack = T.pack
    unpack = T.unpack

instance ToString T.Text where
    string x = T.unpack x    

instance ToLines T.Text where    
    lines = T.lines

instance Formattable T.Text where
    format s = s


