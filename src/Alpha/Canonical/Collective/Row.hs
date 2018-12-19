-----------------------------------------------------------------------------
-- | Defines polymorphic row type following the approach of 
-- Oleg Kiselyov, Ralf Laemmel, Keean Schupke from HList
-- Copyright   :  (c) Oleg Kiselyov, Ralf Laemmel, Keean Schupke, 0xCM
-- License     :  BSD/MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PostfixOperators #-}

module Alpha.Canonical.Collective.Row
(
    Row(..), Label(..)
)
where
import Alpha.Base
import GHC.Read
import Data.List(stripPrefix)

class RowExtend e l where
    type ExtendedRow e l
    (.*.) :: e -> l -> ExtendedRow e l  
    infixr 2 .*.
  
data Label l = Label

instance forall s. KnownSymbol s => Show (Label s) where
    show _ = symstr @s

instance RowExtend (Label x) (Proxy ('[] :: [Type])) where
    type ExtendedRow (Label x) (Proxy ('[] :: [Type])) = Proxy '[x]
    (.*.) _ _ = Proxy

-- Abstract row representative
data family Row (xs::[Type])

data instance Row '[] = ZRow
data instance Row (x ': xs) = x :++: Row xs
infixr 2 :++:

deriving instance Eq (Row '[])
deriving instance (Eq x, Eq (Row xs)) => Eq (Row (x ': xs))

deriving instance Ord (Row '[])
deriving instance (Ord x, Ord (Row xs)) => Ord (Row (x ': xs))

deriving instance Ix (Row '[])
deriving instance (Ix x, Ix (Row xs)) => Ix (Row (x ': xs))

deriving instance Bounded (Row '[])
deriving instance (Bounded x, Bounded (Row xs)) => Bounded (Row (x ': xs))

-- | Proxies for row elements at the value-level
class RowProxies (xs::[Type]) pxs | pxs -> xs, xs -> pxs where 
    proxies :: Row pxs

class DropProxy (AddProxy xs) ~ xs => RowProxies' xs where
    proxies' :: Row (AddProxy xs)
    
instance RowProxies' '[] where
    proxies' = ZRow

instance (RowProxies' xs ) => RowProxies' (x ': xs) where
    proxies' = Proxy :++: proxies'
        
instance RowProxies '[] '[] where
    proxies = ZRow

type family AddProxy(xs :: k) :: k
type instance AddProxy '[] = '[]
type instance AddProxy (x ': xs) = AddProxy x ': AddProxy xs
type instance AddProxy (x :: Type) = Proxy x

type family DropProxy (xs :: k) :: k
type instance DropProxy (x ': xs) = DropProxy x ': DropProxy xs
type instance DropProxy '[] = '[]
type instance DropProxy (Proxy x) = x


instance Show (Row '[]) where
    show _ = "R[]"

instance (Show e, Show (Row r)) => Show (Row(e ': r)) where
    show (x :++: l) = let 'R':'[':s = show l
                        in "R[" <> show x <>
                                    (if s == "]" then s else "," <> s)

instance Read (Row '[]) where
    readsPrec _ str = case stripPrefix "R[]" str of
                        Nothing -> []
                        Just rest -> [(ZRow, rest)]
                                                                    
first :: Row (e ': l) -> e
first (x :++: _) = x

tail :: Row (e ': l) -> Row l
tail (_ :++:  l) = l

class ReverseAppend r1 r2 r3 | r1 r2 -> r3 where
    rappend :: Row r1 -> Row r2 -> Row r3

class Reversal xs sx | xs -> sx, sx -> xs where
    reverse :: Row xs -> Row sx
    
instance ReverseAppend '[] r2 r2 where
    rappend _ r = r

instance ReverseAppend r (x ': r') z => ReverseAppend (x ': r) r' z where
    rappend (x :++: r) r' = rappend r (x :++: r')

instance (ReverseAppend xs '[] sx,
          ReverseAppend sx '[] xs) => Reversal xs sx where
    reverse r = rappend r ZRow


class Inits a b | a -> b, b -> a where
    inits :: Row a -> Row b

class Inits' a b | a -> b, b -> a where
    inits' :: Row a -> Row b
    
instance Inits' a b => Inits a (Row '[] ': b) where
    inits xs = ZRow :++: inits' xs

instance Inits' '[] '[Row '[]] where
    inits' _ = ZRow :++: ZRow
    
-- | evidence to satisfy the fundeps in 'Inits'
type family MapCons (x :: Type) (xxs :: [Type]) :: [Type]
type instance MapCons x (Row a ': b) = Row (x ': a) ': MapCons x b
type instance MapCons x '[] = '[]


type family MapTail (xxs :: [Type]) :: [Type]
type instance MapTail ( Row (a ': as) ': bs) = Row as ': MapTail bs
type instance MapTail '[] = '[]
    
class RowBuilder l r where
    row :: Row l -> r


instance RowBuilder (a ': l) r => RowBuilder l (a->r) where
    row l x = row (x :++: l)
    

-- > let r1 = "a" :++: 'b' :++: 5 :++: ZRow        

    
