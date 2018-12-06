{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
--{-# LANGUAGE UndecidableInstances #-}

module Alpha.Text.Format  where
import qualified Data.List as List
import qualified Data.Text as T
import Alpha.Base
import Alpha.Canonical

-- Lists of showable things are formattable        
instance (Formattable a) => Formattable [a] where
    format x = x |> fmap format |> T.concat

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
            