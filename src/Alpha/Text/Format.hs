module Alpha.Text.Format  where
import Alpha.Base
import Alpha.Canonical
import qualified Alpha.Text.Asci as Asci

import qualified Data.List as List
import qualified Data.Text as T

-- Lists of showable things are formattable        
instance (Formattable a) => Formattable [a] where
    format x = x |> (<$>) format |> T.concat

instance Formattable String where
    format = T.pack 

instance Formattable Int where
    format = T.pack . show
instance Formattable Word where
    format = T.pack . show
instance Formattable Integer where
    format = T.pack . show        
instance Formattable Word8 where
    format = T.pack . show
instance Formattable Word16 where
    format = T.pack . show
instance Formattable Word32 where
    format = T.pack . show
instance Formattable Word64 where
    format = T.pack . show                
instance Formattable Int8 where
    format = T.pack . show
instance Formattable Int16 where
    format = T.pack . show
instance Formattable Int32 where
    format = T.pack . show
instance Formattable Int64 where
    format = T.pack . show
instance Formattable Double where
    format = T.pack . show
instance Formattable Float where
    format = T.pack . show
            
instance (Show a) => Formattable (Set a) where
    format x =  braces (T.pack (show x))
        where braces y = T.append Asci.LBrace (T.append y Asci.RBrace)
                    