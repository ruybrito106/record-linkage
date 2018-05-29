module Record (
    Record,
    rawSet,
    finalSet,
    addr,
) where

type Record = (Int,Float,Float,String)

addr :: Record -> String
addr (_, _, _, address) = address

rawSet :: [Record]
rawSet = [(1, 0.0, 0.0, "Av. Beira Rio"), (2, 0.0, 1.0, "Avenida Beira Rio"), (3, 1.0, 0.0, "Rio Branco Avelino")]

finalSet :: [Int]
finalSet = [1, 3]
