module PowMod where

-- powMod implements the repeated squaring method to quickly calculate x^y mod n
powMod :: Integer -> Integer -> Integer -> Integer
powMod x y n 
  | n <= 0 = error "powMod: non-positive modulus"
  | y < 0 = error "powMod: negative exponent"
  | otherwise = (powModHelp 1 x y) `rem` n where
    -- this code implements the following: if y is odd, then x^y mod n =(x mod n)(x^2 mod n)^((y-1)/2) mod n...
    -- if y is even, the x^y mod n=(x^2 mod n)^(y/2) mod n.
    -- trivially, if y = 0, then x^y mod n = 1
    powModHelp acc x 0 = acc
    powModHelp acc x y =
      if odd y
        then powModHelp (acc * x `rem` n) (x * x `rem` n) (y `quot` 2)
      else powModHelp acc (x * x `rem` n) (y `quot` 2)
