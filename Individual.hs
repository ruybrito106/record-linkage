module Individual (
    Individual,
    cross,
    similarity,
    -- fitness,
) where

data Individual = I Int Float Float
    deriving Show

similarity :: Individual -> Individual -> Float
similarity (I id arg0 arg1) (I id' arg0' arg1') = (2.0 - (abs (arg0-arg0')) - (abs (arg1-arg1'))) / 2.0

cross :: Individual -> Individual -> Individual
cross (I id arg0 arg1) (I id' arg0' arg1') = I id ((arg0 + arg0') / 2.0) ((arg1 + arg1') / 2.0)

-- fitness :: Individual -> Float