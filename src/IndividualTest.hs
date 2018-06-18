module IndividualTest where

import Individual
import Test.QuickCheck

-- Arbitrary data definition

instance Arbitrary Individual where
    arbitrary = do
        x <- genFloat
        x' <- genFloat
        x'' <- genFloat
        let m = 1000.0
        return (newIndividual (-1) (x / m) (x' / m) (x'' / m))

genFloat :: Gen (Float)
genFloat = arbitrary :: Gen (Float)

genIndividual :: Gen (Individual)
genIndividual = arbitrary :: Gen (Individual)

genIndividualArray :: Int -> Gen [Individual]
genIndividualArray n = vectorOf (n) (arbitrary :: Gen (Individual))

-- Auxiliary functions

isFitnessSorted :: [Individual] -> Bool
isFitnessSorted [] = True
isFitnessSorted [x] = True
isFitnessSorted (a:b:as) = (fitness a <= fitness b) && isFitnessSorted (b : as)

crossIntegrity :: Individual -> Individual -> Individual -> Bool
crossIntegrity c a b = c == n
    where
        f = factors a
        f' = factors b
        m = map (\x -> (fst x + snd x) / 2.0) (zip f f')
        n = fromFactors m

-- Properties

prop_sortingByFitness :: Property
prop_sortingByFitness = forAll (genIndividualArray 10) $ (\x -> isFitnessSorted (sortByFitness x))

prop_takingBestKeepsGenerationLength :: Property
prop_takingBestKeepsGenerationLength = forAll (genIndividualArray 10) $ (\x -> length x == length ((takeBest . sortByFitness) x))

prop_crossIntegrity :: Property
prop_crossIntegrity = forAll (genIndividualArray 2) $ (\x -> crossIntegrity (head x) (last x) (cross (head x) (last x)))

checkAll = do 
    quickCheckWith stdArgs { maxSuccess = 5 } prop_sortingByFitness
    quickCheckWith stdArgs { maxSuccess = 5 } prop_takingBestKeepsGenerationLength
    quickCheckWith stdArgs { maxSuccess = 5 } prop_crossIntegrity
    