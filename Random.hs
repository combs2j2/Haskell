module Random where

import PowMod

-- randomGen takes an integer seed, multiplier a, lo and hi bounds, and a length n, and returns a the first n...
-- terms of the pure multiplicative congruential sequence of pseudorandom numbers with x_0=seed 
randomGen :: (Integer, Integer, Integer, Integer, Integer) -> [Integer]
randomGen (seed, a, lo, hi, n) =
  if lo >= hi
    then error "randomGen: hi is not larger than lo"
  else if n <= 0
    then error "randomGen: non-positive length"
  else
    -- pure multiplicative congruential method with x_n=a^n(x_0) where x_0 = seed
    [((seed)*(powMod a x (100000000000000000000000000000000000000000000000151*100000000000000000000000000000000000000000000000447))) `rem` (hi-lo+1) + lo | x <- [1..n]]