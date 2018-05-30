module Record (
    Record,
    rawSet,
    finalSet,
    addr,
    loc,
    ind,
) where

type Record = (Int,Float,Float,String)

addr :: Record -> String
addr (_, _, _, address) = address

loc :: Record -> (Float,Float)
loc (_, lat, lng, _) = (lat,lng)

ind :: Record -> Int
ind (i, _, _, _) = i

rawSet :: [Record]
rawSet = [(1, 0.0, 0.2, "Av. Beira Rio"), (2, 0.0, 0.21, "Avenida Beira Rio"), (3, 0.8, 0.0, "Rio Branco Avelino")]

finalSet :: [(Int, Int)]
finalSet = [(1,2)]
