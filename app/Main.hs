module Main where

import Generation

gens :: Int
gens = 5

evolveN :: Int -> Generation -> IO ()
evolveN 0 _ = return ()
evolveN n gen = do
    x <- return (evolve gen)
    print ("Generation " ++ show (gens - n))
    print (allFit x)
    evolveN (n - 1) x

main :: IO ()
main = do
    putStrLn "Starting evolution..."
    evolveN (gens) (firstGeneration)
