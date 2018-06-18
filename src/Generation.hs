module Generation (
    Generation,
    evolve,
    firstGeneration,
    bestFit,
    allFit,
) where

import Individual

data Generation = G [Individual]
    deriving Show

firstGeneration :: Generation
firstGeneration = G toList

allFit :: Generation -> [Float]
allFit (G []) = []
allFit (G (a:as)) = (fitness a) : (allFit (G as)) 

bestFit :: Generation -> Float
bestFit (G []) = 0.0
bestFit (G (a:as)) = max (fitness a) (bestFit (G as))  

evolve :: Generation -> Generation
evolve (G as) = G (f' as)
    where
        f' = takeBest . sortByFitness