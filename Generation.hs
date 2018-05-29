module Generation (
    -- evolve,
    -- pickBestFit,
    -- completeSet,
) where

import Individual

data Generation = G [Individual]

let genSize = 100
let chosenAmount = 30

-- evolve :: Generation -> Generation
-- pickBestFit :: Generation -> Generation
-- completeSet :: Generation -> Generation