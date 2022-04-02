module MillerRabin where

import PowMod
import Random

-- powers returns the list [n/2^s, n/2^(s-1),..., 2^2, 2^1]
powers :: Integer -> [Integer]
powers n = map (\x -> (n `quot` (2^x))) [1..(findS n)] where
  findS n = findSHelp 0 n where
    findSHelp acc n = 
      if even n 
        then findSHelp (acc+1) (n `div` 2) 
      else acc

-- member function determines if a given element is found in the input list
member :: (Eq a) => a -> [a] -> Bool
member elt [] = False
member elt (x:xs) = 
  if elt == x
    then True 
  else member elt xs

-- millerRabin determines if a number p divides one of the following: (a^((p-1)/2)+1), (a^((p-1)/4)+1),..., (a^((p-1)/(2^s))+1), (a^((p-1)/(2^s))-1)
millerRabin :: Integer -> Integer -> Bool
millerRabin a p = 
  -- if a and p are not relatively prime
  if gcd a p /= 1
    -- return False; a prime must be relatively prime to any number below it
    then False
  -- if p is even and not equal to 2
  else if (even p) && (p /= 2)
    -- return False; 2 is the only even prime
    then False 
  else do
    -- initialize a list of the congruences [(a^((p-1)/2)+1) mod p == 0, (a^((p-1)/4)+1) mod p == 0,..., (a^((p-1)/(2^s))+1) mod p == 0]...
    let testCongs = map (\x -> ((powMod a x p) + 1) `mod` p == 0) (powers (p-1))
    -- if any of these test congruences are true or p divides (a^((p-1)/(2^s))-1)...
    if (member True testCongs) || (((powMod a (last (powers (p-1))) p) - 1) `rem` p == 0)
      -- return true; p might be a prime
      then True
    -- otherwise return False; p is certainly composite
    else False

-- determines if an input number passes an n number of millerRabin tests			
isProbablyPrime :: Integer -> Integer -> Bool
isProbablyPrime 1 _ = False
isProbablyPrime 2 _ = True
isProbablyPrime 3 _ = True
isProbablyPrime 4 _ = False
isProbablyPrime 5 _ = True
isProbablyPrime p n = do
  -- an n number of test bases 'a' are pseudo-randomly generated using the pure multiplicative congruential method from the range (2, p-2)
  let testBases = randomGen (p, p-1, 2, p-2, n)
  -- run the Miller-Rabin test on p using all of these bases
  let allTests = [millerRabin x p | x <- testBases]
  -- if any of these tests return false...
  if (member False allTests)
    -- the p is not prime; return False
    then False
  -- otherwise, p has a probability higher than (1-(1/4)^n) of being prime; return True
  else True

-- generate probable primes with precision n (that is, numbers that pass n randomly selected Miller-Rabin tests) within a specified range
generateProbablePrimes :: (Integer, Integer) -> Integer -> [Integer]
generateProbablePrimes (lo, hi) n =
  if hi < lo
    then error "generateProbablePrimes: upper bound must be at least lower bound"
  else [x | x <- [lo..hi], isProbablyPrime x n]
