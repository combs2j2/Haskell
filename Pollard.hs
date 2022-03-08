module Pollard where

mygcd :: Integer -> Integer -> Integer
mygcd x 0 = x
mygcd 0 x = x
mygcd a b = if a >= b then mygcd b (a `mod` b) else mygcd b a

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

pollardHelp :: Integer -> Integer -> Integer -> Integer
pollardHelp i a n = 
	 if (mygcd (((powMod a i n) `mod` n)-1) n) == 1
		then pollardHelp (i+1) (powMod a i n) n
	else (mygcd (((powMod a i n) `mod` n)-1) n)
	
pollard = pollardHelp 1

getFactorHelp :: Integer -> Integer -> Integer
getFactorHelp a n =
	if (pollard a n) == n || (pollard a n) == -1 
		then (pollard (a+1) n)
	else pollard a n
	
getFactor = getFactorHelp 2