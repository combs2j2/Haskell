module PowMod where

powMod :: Integer -> Integer -> Integer -> Integer
powMod x y n 
  | n <= 0 = error "powMod: non-positive modulus"
  | y < 0 = error "powMod: negative exponent"
  | otherwise = (powModHelp 1 x y) `rem` n where
    powModHelp acc x 0 = acc
    powModHelp acc x y =
      if odd y
        then powModHelp (acc * x `rem` n) (x * x `rem` n) (y `quot` 2)
      else powModHelp acc (x * x `rem` n) (y `quot` 2)