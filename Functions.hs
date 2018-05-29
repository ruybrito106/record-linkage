module Functions (
    editDistanceFactor,
    jaroWinklerFactor,
    haversineFactor,
) where

import Control.Arrow ((***))
import Record

-- Edit Distance
edit :: Eq a => [a] -> [a] -> Int
edit a b 
    = last (if lab == 0 then mainDiag
	    else if lab > 0 then lowers !! (lab - 1)
		    else{- < 0 -}   uppers !! (-1 - lab))
    where mainDiag = oneDiag a b (head uppers) (-1 : head lowers)
	  uppers = eachDiag a b (mainDiag : uppers) 
	  lowers = eachDiag b a (mainDiag : lowers)
	  eachDiag a [] diags = []
	  eachDiag a (bch:bs) (lastDiag:diags) = oneDiag a bs nextDiag lastDiag : eachDiag a bs diags
	      where nextDiag = head (tail diags)
	  oneDiag a b diagAbove diagBelow = thisdiag
	      where doDiag [] b nw n w = []
		    doDiag a [] nw n w = []
		    doDiag (ach:as) (bch:bs) nw n w = me : (doDiag as bs me (tail n) (tail w))
			where me = if ach == bch then nw else 1 + min3 (head w) nw (head n)
		    firstelt = 1 + head diagBelow
		    thisdiag = firstelt : doDiag a b firstelt diagAbove (tail diagBelow)
	  lab = length a - length b
          min3 x y z = if x < y then x else min y z
          
editDistanceFactor :: Record -> Record -> Float
editDistanceFactor r0 r1 = 1.0 - (fromIntegral (edit (addr r0) (addr r1)) / max (fromIntegral (length (addr r0))) (fromIntegral (length (addr r1))))
          
-- Jaro Winkler            
jaro :: String -> String -> Float
jaro "" _ = 0.0
jaro _ "" = 0.0
jaro s1 s2
	| s1 == s2 = 1.0
	| s1 /= s2 =
		let
			l1 = length s1
			l2 = length s2
			z2 = zip [1..] s2
			m = foldl (++) [] [charMatch p ((max l1 l2) `div` 2) z2 | p <- zip [1..] s1]
			ml = length m
			t = sum [realToFrac (transposition p z2) / 2.0 | p <- m]
			ml1 = realToFrac ml / realToFrac l1
			ml2 = realToFrac ml / realToFrac l2
			mtm = (realToFrac ml - t) / realToFrac ml
		in
			(1 / 3) * (ml1 + ml2 + mtm)
		where
			charMatch (p,q) far list = filter (\(x,y) -> x >= p - far && x <= p + far && y == q) list
			transposition (p,q) list = length $ filter (\(x,y) -> p /= x && q == y) list

commonPrefix :: String -> String -> String
commonPrefix a "" = []
commonPrefix "" b = []
commonPrefix (a:as) (b:bs)
    | a == b = a : commonPrefix as bs
    | otherwise = ""

winkler :: String -> String -> Float -> Float
winkler "" _ _ = 0.0
winkler _ "" _ = 0.0
winkler s1 s2 jaro
	| s1 == s2 = 1.0
	| s1 /= s2 =
		let
			l = length $ commonPrefix s1 s2
			p = 0.1
		in
			jaro + ((realToFrac l * p) * (1.0 - jaro))


jaroWinklerFactor :: String -> String -> Float
jaroWinklerFactor "" _ = 0.0
jaroWinklerFactor _ "" = 0.0
jaroWinklerFactor s1 s2
	| s1 == s2 = 1.0
    | s1 /= s2 = winkler s1 s2 $ jaro s1 s2
    
-- Haversine Distance
angToHaversine :: Float -> Float
angToHaversine = (^ 2) . sin . (/ 2)
 
haversineFactor :: (Float, Float) -> (Float, Float) -> Float
haversineFactor = distDeg 6371
  where
    distDeg radius p1 p2 = distRad radius (deg2rad p1) (deg2rad p2)
    distRad radius (lat1, lng1) (lat2, lng2) =
      (2 * radius) *
      asin
        (min
           1.0
           (sqrt $
           angToHaversine (lat2 - lat1) +
            ((cos lat1 * cos lat2) * angToHaversine (lng2 - lng1))))
    deg2rad = d2r *** d2r
      where
        d2r = (/ 180) . (pi *)