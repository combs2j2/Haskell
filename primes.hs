isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

numsBelowSquareRoot :: Int -> [Int]
numsBelowSquareRoot n = [2..isqrt n]

isPrime :: Int -> Bool
isPrime 1 = False
isPrime 0 = False
isPrime 2 = True
isPrime n = if myLen [x | x <- numsBelowSquareRoot n, mod n x == 0] == 0 then True else False

listOfPrimes = [x | x <- [2..], isPrime x]

numPrimesUpTo :: Int -> Int
numPrimesUpTo 0 = 0
numPrimesUpTo 1 = 0
numPrimesUpTo 2 = 1
numPrimesUpTo n = if isPrime n then 1 + numPrimesUpTo (n-1) else numPrimesUpTo (n-1)

densityOfPrimes :: Int -> Rational
densityOfPrimes 0 = 0
densityOfPrimes 1 = 0
densityOfPrimes n = b / a where 
    b = fromIntegral (numPrimesUpTo n) :: Rational
    a = fromIntegral n :: Rational	

relativelyPrime :: Int -> Int -> Bool
relativelyPrime a b = if (gcd a b) == 1 then True else False

eulerPhi :: Int -> Int
eulerPhi n = length [x | x <- [1..n], relativelyPrime x n]