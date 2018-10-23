{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Alpha.Text.Format 
(
    Formattable(..),
    Show'(..),
    shows'
)
where
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


------------------------------------------------------------------------
-- Show'
-- | A parameterized type that can be shown on all instances.
--
-- To implement @'ShowF' g@, one should implement an instance @'Show'
-- (g tp)@ for all argument types @tp@, then write an empty instance
-- @instance 'ShowF' g@.
class Show' (f :: k -> *) where
    -- | Provides a show instance for each type.
    withShow :: p f -> q tp -> (Show (f tp) => a) -> a
  
    default withShow :: Show (f tp) => p f -> q tp -> (Show (f tp) => a) -> a
    withShow _ _ x = x
  
    show' :: forall tp . f tp -> String
    show' x = withShow (Proxy :: Proxy f) (Proxy :: Proxy tp) (show x)
  
    -- | Like 'showsPrec', the precedence argument is /one more/ than the
    -- precedence of the enclosing context.
    showsPrec' :: forall tp. Int -> f tp -> String -> String
    showsPrec' p x = withShow (Proxy :: Proxy f) (Proxy :: Proxy tp) (showsPrec p x)
  
shows' :: Show' f => f tp -> String -> String
shows' x = showsPrec' 0 x

instance Show x => Show' (Const x)