module MillerRabin where

-- Source code from Data.Mod used for fast modular exponentiation
powMod :: (Integral a, Integral b) => a -> b -> a -> a
powMod x y m
  | m <= 0    = error "powModInt: non-positive modulo"
  | y <  0    = error "powModInt: negative exponent"
  | otherwise = f (x `rem` m) y 1 `mod` m
  where
    f _ 0 acc = acc
    f b e acc = f (b * b `rem` m) (e `quot` 2)
      (if odd e then b * acc `rem` m else acc)
	  
-- Given a number n, determine s such that n-1=(2^s)d where d is an odd integer
findSHelp :: Integer -> Integer -> Integer
findSHelp acc n = if n `mod` 2 == 0 then findSHelp (acc+1) (n `div` 2) else acc

findS = findSHelp 0

-- powers determines 
powers :: Integer -> [Integer]
powers n = map (\x -> (n `div` (2^x))) [1..((findS n))]

-- member function determines if a given element is found in the input list
member :: (Eq a) => a -> [a] -> Bool
member elt [] = False
member elt (x:xs) = if elt == x then True else member elt xs

millerRabin :: Integer -> Integer -> Bool
millerRabin a p = if gcd a p /= 1 then False else
	if (p `mod` 2 == 0) then False else do
	let testCongs = map (\x -> ((powMod a x p) + 1) `mod` p == 0) (powers (p-1))
	if (member True testCongs) || (((powMod a (last (powers (p-1))) p) - 1) `mod` p == 0) then True else False
