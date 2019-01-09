module Alpha.Data.Markov
(
    DecisionProcess(..),

)
where
import Alpha.Canonical
import Alpha.Data.StateSpace
import Alpha.Data.Probability


-- | Represents a Markov decision process
newtype DecisionProcess l s a = DecisionProcess ([State l s], [Rule l s a])

instance Stochastic (Reaction l t s a) where
    probability t p = Probability (t, check p) where
        
        check::(OrdNum p) => p -> p
        check p = case (p >= 0 && p <= 1) of
                         True -> p
                         _ -> error "Out of bounds" 


