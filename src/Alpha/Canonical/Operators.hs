module Alpha.Canonical.Operators where
import Alpha.Base

-- | If the first input value is true, returns the 2nd input value,
-- otherwise, returns the third input value
ifelse::Bool -> a -> a -> a
ifelse x aye no = case x of
            True -> aye
            _ -> no

-- | Constructs a left-valued 'Either'
left :: l -> Either l r
left x = Left x

-- | Constructs a right-valued 'Either'
right :: r -> Either l r
right x = Right x

map::(Functor f) => (a -> b) -> f a -> f b
map = fmap

-- | The forward pipe operator
infixl 0 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x

-- | The backward pipe operator
infixr 0 <|
(<|) :: (a -> b) -> a -> b
f <| x = f x

-- empty::(Monoid m) => m
-- empty = mempty

symstr :: forall s. KnownSymbol s => String
symstr = symbolVal @s Proxy
