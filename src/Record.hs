module Record (
    Record,
    rawSet,
    finalSet,
    finalSetTransitiveClosure,
    membershipFinalSet,
    singles,
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
rawSet = [
    (1, -23.217637999999997, -45.895517999999996, "Avenida Andrômeda, Jardim Satélite - São José dos Campos"),
    (2, -23.217565, -45.895427, "Av. Andrômeda, 100, Jardim Satélite - São José dos Campos"),
    (3, -21.781539, -43.361705, "Av Presidente Itamar Franco, 3600, São Mateus"),
    (4, -21.780761, -43.360141, "Av. Pres. Itamar Franco, 3600, São Mateus"),
    (5, 42.626225999999996, -82.990696, "Hall Road - Sterling Heights - Michigan"),
    (6, 42.626155, -82.99061499999999, "13835 Lakeside Cir - Sterling Heights - Michigan"),
    (7, -22.882219, -48.451831, "Av. Dr. Vital Brasil, 771, Vila Sao Lucio"),
    (8, -22.882195, -48.451679, "Avenida Doutor Vital Brasil, Vila Sao Lucio"),
    (9, -22.964557, -43.222300999999995, "Rua Von Martius, Jardim Botânico"),
    (10, -8.077838, -34.919402999999996, "Rua Dr João Elísio, 327 em Mangueira, Mangueira"),
    (11, 40.92979, -74.068046, "26 Broad Ave - Paramus"),
    (12, 33.938739, -117.250686, "23631 Sunnymead Blvd - Moreno Valley"),
    (13, -12.098327, -76.95082, "Alameda el corregidor cuadra 15, La Molina"),
    (14, -22.727753999999997, -47.172508, "Rua Onze, 71, Cascata"),
    (15, -19.842924, -43.977534999999996, "Av Guarapari, 466, Santa Amelia"),
    (16, -23.217, -45.8955, "Avenida Andrômeda, 100, Jardim Paulista - São Paulo"),
    (17, -21.78, -43.36, "Av. Presidente It. Franco, 3601, São Judas"),
    (18, 40.92978, -74.06805, "32 Broad Ave - Paramus"),
    (19, 40.92976, -74.068048, "46 Broad Avenue - Paramus"),
    (20, 40.929761, -74.068049, "22 Broad Avenue - Paramus"),
    (21, -21.781539, -43.361705, "P. Itamar Franco Avenida, 3600, S. Mateus"),
    (22, -21.780761, -43.360141, "Av. Pres. Itamar Franco, 3600")
    ]

finalSet :: [(Int, Int)]
finalSet = [(1,2), (1, 21), (1,22), (2,21), (2,22), (21,22), (3,4), (5,6), (7,8)]

finalSetTransitiveClosure :: [(Int, Int)]
finalSetTransitiveClosure = finalSet ++ (map (\x -> (snd x, fst x)) (finalSet))

membershipFinalSet :: (Int, Int) -> [(Int, Int)] -> Bool
membershipFinalSet _ [] = False
membershipFinalSet x (a:as) = (x == a) || (membershipFinalSet x as)

singles :: [Int]
singles = [9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
