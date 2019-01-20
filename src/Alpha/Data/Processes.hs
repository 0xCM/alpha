-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) Chris Moore, 2018
-- License     :  MIT
-- Maintainer  :  0xCM00@gmail.com
-----------------------------------------------------------------------------
module Alpha.Data.Processes
(
    DecisionProcess(..),

)
where
import Alpha.Canonical
import Alpha.Data.Machinery



-- | Represents a Markov decision process
newtype DecisionProcess l s a = DecisionProcess ([State l s], [Rule l s a])

-- | Characterizes probabalistic events
class Stochastic e  where
    -- | Assigns the probability of the event e to the value p
    probability::(OrdNum p) => e -> p -> Probability e p

            

-- | Represents the probability 'p' of the occurrence of an event 'e'
newtype Probability e p = Probability (e, p)
    deriving(Eq, Ord, Generic, Data, Typeable)

instance Stochastic (Reaction l t s a) where
    probability t p = Probability (t, check p) where
        
        check::(OrdNum p) => p -> p
        check p = case (p >= 0 && p <= 1) of
                         True -> p
                         _ -> error "Out of bounds" 



-- | Represents a distribution-parameterized random variable
newtype RVar d a = RVar d

-- | Represents a stochastic process, a time-indexed collection 
-- of random variables [Y2008DES]
newtype StochasticProcess d a t = StochasticProcess [(t, RVar d a)]

