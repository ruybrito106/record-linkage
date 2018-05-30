module Generation (
    -- evolve,
    -- pickBestFit,
    -- completeSet,
) where

import Individual

data Generation = G [Individual]

genSize :: Int
genSize = 100

chosenAmount :: Int
chosenAmount = 30

-- evolve :: Generation -> Generation
-- pickBestFit :: Generation -> Generation
-- completeSet :: Generation -> Generation