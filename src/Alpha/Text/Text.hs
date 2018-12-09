module Alpha.Text.Text where

--import Text.Show
import qualified Data.Text as T
import qualified Data.String as S
import Alpha.Canonical
import Alpha.Base

    
instance Packable S.String T.Text where
    pack = T.pack
    unpack = T.unpack

instance Reversible T.Text T.Text where    
    reverse = T.reverse
        
instance Cloneable Int Text where
    type Cloned Int Text = Text
    clone = T.replicate
    

instance Weavable Char Text where
    weave = T.intersperse
    