module Generation (
    evolve,
) where

import Individual

data Generation = G [Individual]
    deriving Show

firstGeneration :: Generation
firstGeneration = G toList

bestFit :: Generation -> Float
bestFit (G []) = 0.0
bestFit (G (a:as)) = max (fitness a) (bestFit (G as))  

evolve :: Generation -> Generation
evolve (G as) = G (f' as)
    where
        f' = takeBest . sortByFitness