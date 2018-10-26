{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Alpha.Text.Format  where
import qualified Data.List as List
import qualified Data.Text as T
import Alpha.Base
import Alpha.Canonical

-- Lists of showable things are formattable        
instance (Show a) => Formattable [a] where
    format x = T.pack (show x)

instance Formattable String where
    format = T.pack 
    