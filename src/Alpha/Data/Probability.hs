module Alpha.Data.Probability
(
    Probability(..),
    Stochastic(..),    

)
where
import Alpha.Canonical


-- | Represents the probability 'p' of the occurrence of an event 'e'
newtype Probability e p = Probability (e, p)
    deriving(Eq, Ord)

-- | Characterizes probabalistic events
class Stochastic e  where
    -- | Assigns the probability of the event e to the value p
    probability::(OrdNum p) => e -> p -> Probability e p

            
