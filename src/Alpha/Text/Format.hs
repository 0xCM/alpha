{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module Alpha.Text.Format  where
import Data.Set(Set)
import Data.Text(Text)
import Data.String(String)
import Data.Char(Char)
import Data.ByteString(ByteString)
import Data.Int(Int)
import qualified Data.List as List
import Data.Functor.Const
import GHC.Show
import Alpha.Canonical

import qualified Data.Text as T
import qualified Data.String as S
import qualified Alpha.Data.Asci as Ascii
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LC8    
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as Text
        
-- Sets of showable things are formattable            
instance (Show a) => Formattable (Set a) where
    format x =  wrap (T.pack (show x))
        where wrap y = T.append Ascii.LBrace (T.append y Ascii.RBrace)

-- Lists of showable things are formattable        
instance (Show a) => Formattable [a] where
    format x = T.pack (show x)

instance Formattable String where
    format = T.pack 
    
