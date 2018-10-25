-----------------------------------------------------------------------------
-- | Selected definitions from the 'singletons' library
-- Copyright   :  (c) 0xCM, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-- Stability   :  experimental
-- Portability :  portable
-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Alpha.TypeLevel.Singletons
(
    Sing
)

where
import Data.Kind (Type)
import Data.Type.Equality hiding (type (==), apply)
import Data.Proxy
import Data.Eq
import Data.Void
import Data.Ord
import qualified Data.List as List
import GHC.Show
import Alpha.Canonical hiding ((-),Order,Nat)
import GHC.Exts (Constraint)
import Alpha.TypeLevel.Proxy


data family Sing :: k -> Type

-- | Because we can never create a value of type 'Void', a function that type-checks
-- at @a -> Void@ shows that objects of type @a@ can never exist. Thus, we say that
-- @a@ is 'Refuted'
type Refuted a = (a -> Void)

-- | A 'Decision' about a type @a@ is either a proof of existence or a proof that @a@
-- cannot exist.
data Decision a = Proved a               -- ^ Witness for @a@
                | Disproved (Refuted a)  -- ^ Proof that no @a@ exists

-- | Members of the 'SDecide' "kind" class support decidable equality. Instances
-- of this class are generated alongside singleton definitions for datatypes that
-- derive an 'Eq' instance.
class SDecide k where
  -- | Compute a proof or disproof of equality, given two singletons.
  (%~) :: forall (a :: k) (b :: k). Sing a -> Sing b -> Decision (a :~: b)
  infix 4 %~

-- | A 'SingI' constraint is essentially an implicitly-passed singleton.
-- If you need to satisfy this constraint with an explicit singleton, please
-- see 'withSingI' or the 'Sing' pattern synonym.
class SingI a where
    -- | Produce the singleton explicitly. You will likely need the @ScopedTypeVariables@
    -- extension to use this method the way you want.
    sing :: Sing a

-- | An /existentially-quantified/ singleton. This type is useful when you want a
-- singleton type, but there is no way of knowing, at compile-time, what the type
-- index will be. To make use of this type, you will generally have to use a
-- pattern-match:
--
-- > foo :: Bool -> ...
-- > foo b = case toSing b of
-- >           SomeSing sb -> {- fancy dependently-typed code with sb -}
--
-- An example like the one above may be easier to write using 'withSomeSing'.
data SomeSing k where
    SomeSing :: Sing (a :: k) -> SomeSing k
  
-- | Force GHC to unify the kinds of @a@ and @b@. Note that @SameKind a b@ is
-- different from @KindOf a ~ KindOf b@ in that the former makes the kinds
-- unify immediately, whereas the latter is a proposition that GHC considers
-- as possibly false.
type SameKind (a :: k) (b :: k) = (() :: Constraint)

class SingKind k where
    -- | Get a base type from the promoted kind. For example,
    -- @Demote Bool@ will be the type @Bool@. Rarely, the type and kind do not
    -- match. For example, @Demote Nat@ is @Natural@.
    type Demote k = (r :: Type) | r -> k
    
    -- | Convert a singleton to its unrefined version.
    fromSing :: Sing (a :: k) -> Demote k
    
    -- | Convert an unrefined type to an existentially-quantified singleton type.
    toSing   :: Demote k -> SomeSing k
      
-- Singleton type equality type class
class SEq a where
    (%==) :: Sing (x :: a) -> Sing (y :: a) -> Sing (x == y)
    
-- Promoted equality type class
class PEq a where
    type (==) (x :: a) (y :: a) :: Bool
    type (/=) (x :: a) (y :: a) :: Bool
  
    type x == y = Not (x /= y)
    type x /= y = Not (x == y)
    
instance PEq a => PEq (Maybe a) where
    type m1 == m2 = EqualsMaybe m1 m2
                
type SNat (n::Nat) = Sing n

type family IsZero (n :: Nat) :: Bool where
    IsZero n = If (n == Zero) True False

infixl 6 +    
type family a + b where
    Zero + b = b
    Next a + b = Next (a + b)
      
type family x :*: y

type family (a :: Nat) :+: (b :: Nat) :: Nat
type instance Zero   :+: n = n
type instance Next n :+: m = Next (n :+: m)

type family (a :: Bool) || (b :: Bool) :: Bool where
    False || x = x
    True || x = True

type False = 'False
type True = 'True

type family (-) (m :: Nat) (n :: Nat) :: Nat where
    Zero - x = Zero
    (Next x) - Zero = Next x
    (Next x) - (Next y) = x - y

type family Not (x :: Bool) :: Bool where
    Not True = False
    Not False = True
      
type family If cond tru fls where
    If True  tru  fls = tru
    If False tru  fls = fls

type family (a :: Bool) && (b :: Bool) :: Bool where
    False && _ = False
    True  && x = x
    
type family EqualsMaybe (a :: Maybe k) (b :: Maybe k) where
    EqualsMaybe Nothing Nothing = True
    EqualsMaybe (Just a) (Just a') = a == a'
    EqualsMaybe (x :: Maybe k) (y :: Maybe k) = False
          
type family Contains (a :: k) (b :: List k) :: Bool where
    Contains elt Nil = False
    Contains elt (Cons h t) = (elt == h) || (Contains elt t)
      
data GEq :: Nat -> Nat -> Type where
    GeZ :: GEq y Zero
    GeS :: GEq x y -> GEq (Next x) (Next y)
    
data Order :: Nat -> Nat -> Type where
    Ge :: GEq x y -> Order x y
    Le :: GEq y x -> Order x y
          
order' :: SNat a -> SNat b -> Order a b
order'  _         SZero    = Ge GeZ
order'  SZero    (SNext _) = Le GeZ
order' (SNext a) (SNext b) = case order' a b of
                                Ge y -> Ge (GeS y)
                                Le y -> Le (GeS y)
        
data Nat :: Type where
    Zero :: Nat
    Next :: Nat -> Nat
    deriving Eq
  
data Bool :: Type where
    False :: Bool
    True :: Bool
    
data Maybe :: Type -> Type where
    Nothing :: Maybe a
    Just :: a -> Maybe a
    deriving Eq
      
data List :: Type -> Type where
    Nil :: List a
    Cons :: a -> List a -> List a
    deriving Eq
            
data Either :: Type -> Type -> Type where
    Left :: a -> Either a b
    Right :: b -> Either a b

data Empty
data instance Sing :: Empty -> Type

instance SingKind Empty where
    type Demote Empty = Empty
    
    fromSing = \case
    toSing x = SomeSing (case x of)
    
infixr 5 :>
data Vec :: Nat -> Type -> Type where
    VNil:: Vec Zero a
    (:>) :: a -> Vec n a -> Vec (Next n) a
        
deriving instance Show a => Show (Vec n a)

data Composite :: Type -> Type -> Type where
    Compose :: Either (Maybe a) b -> Composite a b
  
data instance Sing :: Composite k1 k2 -> Type where
    SCompose :: forall k1 k2 (a :: Either (Maybe k1) k2). Sing a -> Sing (Compose a)  

data instance Sing :: Bool -> Type where
    SFalse :: Sing False
    STrue :: Sing True
    
data instance Sing :: Ordering -> Type where
    SLT :: Sing 'LT
    SEQ :: Sing 'EQ
    SGT :: Sing 'GT
      
data instance Sing :: Maybe k -> Type where
    SNothing :: Sing Nothing
    SJust :: forall k (a :: k). Sing a -> Sing (Just a)      

data instance Sing :: Nat -> Type where
    SZero :: Sing Zero
    SNext :: Sing n -> Sing (Next n)
    
data instance Sing :: List k -> Type where
    SNil :: Sing Nil
    SCons :: forall k (h :: k) (t :: List k). Sing h -> Sing t -> Sing (Cons h t)    
        
type family EqualsList (a :: List k) (b :: List k) where
    EqualsList Nil Nil = True
    EqualsList (Cons a b) (Cons a' b') = (a == a') && (b == b')
    EqualsList (x :: List k) (y :: List k) = False

instance PEq a => PEq (List a) where
    type l1 == l2 = EqualsList l1 l2

instance SEq k => SEq (List k) where
    SNil %== SNil = STrue
    SNil %== (SCons _ _) = SFalse
    (SCons _ _) %== SNil = SFalse
    (SCons a b) %== (SCons a' b') = (a %== a') %&& (b %== b')

instance SDecide k => SDecide (List k) where
    SNil %~ SNil = Proved Refl
    (SCons h1 t1) %~ (SCons h2 t2) =
        case (h1 %~ h2, t1 %~ t2) of
        (Proved Refl, Proved Refl) -> Proved Refl
        (Disproved contra, _) -> Disproved (\Refl -> contra Refl)
        (_, Disproved contra) -> Disproved (\Refl -> contra Refl)
    SNil %~ (SCons _ _) = Disproved (\case)
    (SCons _ _) %~ SNil = Disproved (\case)
      
--intvec = 5 :> 3 :> 8 :> VNil

mul :: Proxy x -> Proxy y -> Proxy (x :*: y)
mul _ _ = proxy

infix 6 `sadd`
sadd :: SNat a -> SNat b -> SNat (a :+: b)
SZero     `sadd` m = m
(SNext n) `sadd` m = SNext (n `sadd` m)

-- append :: Vector n a -> Vector m a -> Vector (n + m) a
-- append VNil v = v
-- append (VCons a as) v = VCons a (append as v)  

(||) :: Bool -> Bool -> Bool
False || x = x
True || _ = True

(&&) :: Bool -> Bool -> Bool
False && _ = False
True  && x = x

sNot :: Sing b -> Sing (Not b)
sNot STrue = SFalse
sNot SFalse = STrue


(-) :: Nat -> Nat -> Nat
Zero - _ = Zero
(Next x) - Zero = Next x
(Next x) - (Next y) = x - y
    
(%&&) :: forall (a :: Bool) (b :: Bool). Sing a -> Sing b -> Sing (a && b)
SFalse %&& SFalse = SFalse
SFalse %&& STrue = SFalse
STrue %&& SFalse = SFalse
STrue %&& STrue = STrue

(%||) :: Sing a -> Sing b -> Sing (a || b)
SFalse %|| x = x
STrue %|| _ = STrue

sIf :: Sing a -> Sing b -> Sing c -> Sing (If a b c)
sIf STrue b _ = b
sIf SFalse _ c = c

safeHead :: Vec (Next n) a -> a
safeHead (x :> _) = x

safeTail :: Vec (Next n) a -> Vec n a
safeTail (_ :> xs) = xs

snoc :: Vec n a -> a -> Vec (Next n) a
snoc VNil       x = x :> VNil
snoc (y :> ys) x = y :> snoc ys x

reverse :: Vec n a -> Vec n a
reverse VNil       = VNil
reverse (x :> xs) = snoc (reverse xs) x

infixr 5 ++
(++) :: Vec n a -> Vec m a -> Vec (n + m) a
VNil ++ ys       = ys
(x :> xs) ++ ys = x :> (xs ++ ys)

replicate :: SNat n -> a -> Vec n a
replicate SZero      _ = VNil
replicate (SNext n') x = x :> replicate n' x    
    
isZero :: Nat -> Bool
isZero n = if n == Zero then True else False

pred :: Nat -> Nat
pred Zero = Zero
pred (Next n) = n

sContains :: forall a (t1 :: a) (t2 :: List a). SEq a => Sing t1
          -> Sing t2 -> Sing (Contains t1 t2)
sContains _ SNil =
  let lambda :: forall wild. Sing (Contains wild Nil)
      lambda = SFalse
  in
  lambda
sContains elt (SCons h t) =
  let lambda :: forall elt h t. (elt ~ t1, (Cons h t) ~ t2) => Sing elt -> Sing h -> Sing t -> Sing (Contains elt (Cons h t))
      lambda elt' h' t' = (elt' %== h') %|| sContains elt' t'
  in
  lambda elt h t

type Z0 = Sing 0
type Z1 = Sing Z0
type Z2 = Sing Z1
type Z3 = Sing Z2
type Z4 = Sing Z3
type Z5 = Sing Z4
type Z6 = Sing Z5
type Z7 = Sing Z6
type Z8 = Sing Z7
type Z9 = Sing Z8
type Z10 = Sing Z9
type Z11 = Sing Z10
type Z12 = Sing Z11
type Z13 = Sing Z12
type Z14 = Sing Z13
type Z15 = Sing Z14

