module Individual (
    Individual,
    cross,
    similarity,
    -- fitness,
) where

import Record
import Functions

data Individual = I Int Float Float Float
    deriving Show

threshold :: Float
threshold = 0.85

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
genEdges i rs = genEdges' (i) (genPairs rs rs) 
