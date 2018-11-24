module Alpha.Text.Text where

import Text.Show
import qualified Data.Text as T
import qualified Data.String as S
import Alpha.Canonical
import Alpha.Data.Numbers
import Data.Text(Text)
import Data.Char(Char)
import qualified Data.List as List

instance Length T.Text where
    length t =   convert (T.length t)
    
instance Packable S.String T.Text where
    pack = T.pack
    unpack = T.unpack

instance ToString T.Text where
    string x = T.unpack x    

instance ToLines T.Text where    
    lines = T.lines

instance Reversible T.Text T.Text where    
    reverse = T.reverse
    
instance Formattable T.Text where
    format s = s

instance Concatenable Text Text where    
    type Concatenated Text Text = Text
    concat  = T.append

instance Appendable Text Text where
    type Appended Text Text = Text
    append = T.append    

instance Prependable Text Text where
    type Prepended Text Text = Text
    prepend x y = T.append y x 

        
instance Concatenable Text Char where
    type Concatenated Text Char = Text
    concat t c  = T.pack  [c] |> T.append t 
    
instance Concatenable Char Text where    
    type Concatenated Char Text = Text
    concat c t  = T.append (T.pack [c]) t
    
instance Concatenable Char Char where    
    type Concatenated Char Char = Text
    concat c1 c2  = T.pack ([c1] List.++ [c2])
    
