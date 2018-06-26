module Individual (
    Individual,
    cross,
    factors,
    fromFactors,
    similarity,
    toList,
    sortByFitness,
    takeBest,
    fitness,
    fitness',
    newIndividual,
    allFitness,
    allFitness',
    describe,
) where

import Record
import Functions
import qualified Data.IntDisjointSet as DSet
import Data.List

data Individual = I Int Float Float Float
    deriving Show

instance Eq Individual where
    a == b = fitness' a == fitness' b

instance Ord Individual where
    a <= b = fitness' a <= fitness' b

threshold :: Float
threshold = 0.90

newIndividual :: Int -> Float -> Float -> Float -> Individual
newIndividual x a b c  = I x a b c

factors :: Individual -> [Float]
factors (I x a b c) = [a, b, c]

fromFactors :: [Float] -> Individual
fromFactors (a:b:as) = newIndividual (-1) (a) (b) (head as)

toList :: [Individual]
toList = [I (-1) (a) (b) ((a + b)/2.0) | a <- as, b <- bs]
    where
        as = map (\x -> x / 4.05) ([0..4] :: [Float])
        bs = map (\x -> x / 3.25) ([3, 2..1] :: [Float])

sortByFitness :: [Individual] -> [Individual]
sortByFitness l = sort l

takeBest :: [Individual] -> [Individual]
takeBest as = low ++ avg ++ best
    where
        len = length as
        best = drop (quot (80 * len) (100)) as -- 20% best
        avg = crossBestFit (drop (quot (70 * len) (100)) (as)) -- 30% best mixed
        low = take (len - (length best) - (length avg)) (reverse as)

crossBestFit :: [Individual] -> [Individual]
crossBestFit [] = []
crossBestFit [a] = [a]
crossBestFit (a:b:as) = (cross (a) (b)) : (crossBestFit as) 

similarity :: Individual -> Individual -> Float
similarity (I id arg0 arg1 arg2) (I id' arg0' arg1' arg2') = (3.0 - diff) / 3.0
    where 
        diff0 = abs (arg0-arg0')
        diff1 = abs (arg1-arg1')
        diff2 = abs (arg2-arg2')
        diff = diff0 + diff1 + diff2

cross :: Individual -> Individual -> Individual
cross (I id arg0 arg1 arg2) (I id' arg0' arg1' arg2') = I id mean0 mean1 mean2
    where
        mean0 = (arg0+arg0') / 2.0
        mean1 = (arg1+arg1') / 2.0
        mean2 = (arg2+arg2') / 2.0

areMergable :: Individual -> Record -> Record -> Bool
areMergable (I _ f0 f1 f2) a b = (ind a /= ind b) && m >= threshold 
    where
        f0' = f0 * (editDistanceFactor a b)
        f1' = f1 * (jaroWinklerFactor a b)
        f2' = f2 * (haversineFactor a b)
        m = (f0' + f1' + f2') / 3.0 

genPairs :: [Record] -> [Record] -> [(Record, Record)]
genPairs a b = [(x,y) | x <- a, y <- b]

genEdges' :: Individual -> [(Record, Record)] -> [(Int,Int)]
genEdges' i [] = []
genEdges' i (p:ps)
    | areMergable i (fst p) (snd p) = (ind (fst p), ind (snd p)) : (genEdges' i ps)
    | otherwise = genEdges' i ps

genEdges :: Individual -> [Record] -> [(Int,Int)]
genEdges i rs = filter (\x -> fst x < snd x) (genEdges' (i) (genPairs rs rs)) 

-- Using disjoint sets

truePositives :: [(Int,Int)] -> Int
truePositives [] = 0
truePositives (a:as) 
    | fst (DSet.equivalent (fst a) (snd a) (ds)) = 1 + truePositives as
    | otherwise = truePositives as
    where
        ds = DSet.fromList finalSet

falseNegatives :: [(Int, Int)] -> Int
falseNegatives l = (length finalSet) - (truePositives l)

falsePositives :: [(Int, Int)] -> Int
falsePositives [] = 0
falsePositives (a:as) 
    | (xor (contains (fst (a)) (singles)) (contains (snd (a)) (singles))) || fst (DSet.equivalent (fst a) (snd a) (ds)) == False = 1 + falsePositives as
    | otherwise = falsePositives as
    where
        contains x [] = False
        contains x (a:as) = x == a || (contains x as)
        xor a b = (a == False && b == True) || (a == True && b == False)
        ds = DSet.fromList finalSet

trueNegatives :: [(Int, Int)] -> Int
trueNegatives l = (quot (x * (x - 1)) (2)) + (x * 2 * y) - (falsePositives l) 
    where
        x = length singles
        y = length finalSet

-- Using transitive closure

truePositives' :: [(Int,Int)] -> Int
truePositives' [] = 0
truePositives' (a:as) 
    | membershipFinalSet (a) (finalSetTransitiveClosure)  = 1 + truePositives' as
    | otherwise = truePositives' as

falsePositives' :: [(Int, Int)] -> Int
falsePositives' [] = 0
falsePositives' (a:as) 
    | (xor (contains (fst (a)) (singles)) (contains (snd (a)) (singles))) || membershipFinalSet (a) (finalSetTransitiveClosure) == False = 1 + falsePositives' as
    | otherwise = falsePositives' as
    where
        contains x [] = False
        contains x (a:as) = x == a || (contains x as)
        xor a b = (a == False && b == True) || (a == True && b == False)

describe :: Individual -> ([(Int, Int)], [Int])
describe i = (edges, [tn, fp, tp, fn])
    where
        edges = genEdges i rawSet
        tn = fromIntegral (trueNegatives edges)
        fp = fromIntegral (falsePositives edges)
        tp = fromIntegral (truePositives edges)
        fn = fromIntegral (falseNegatives edges)

fitness :: Individual -> Float
fitness i 
    | (fn + tp) * (tn + fp) == 0.0 = 0.0
    | otherwise = ((tp * (tn + fp)) + (tn * (fn + tp))) / (2 * (fn + tp) * (tn + fp))  
    where
        edges = genEdges i rawSet
        tn = fromIntegral (trueNegatives edges)
        fp = fromIntegral (falsePositives edges)
        tp = fromIntegral (truePositives edges)
        fn = fromIntegral (falseNegatives edges)


fitness' :: Individual -> Float
fitness' i 
    | (fn + tp) * (tn + fp) == 0.0 = 0.0
    | otherwise = ((tp * (tn + fp)) + (tn * (fn + tp))) / (2 * (fn + tp) * (tn + fp))  
    where
        edges = genEdges i rawSet
        tn = fromIntegral (trueNegatives edges)
        fp = fromIntegral (falsePositives' edges)
        tp = fromIntegral (truePositives' edges)
        fn = fromIntegral (falseNegatives edges)

allFitness :: [Individual] -> [Float]
allFitness l = map (\x -> fitness x) l

allFitness' :: [Individual] -> [Float]
allFitness' l = map (\x -> fitness' x) l

-- tp / (tp + fn)
-- tn / (tn + fp)