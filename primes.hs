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

go :: Int -> Int -> Int
go a 0 = a
go a n = if isPrime n then go (a+1) (n-1) else go a (n-1)

numPrimesUpTo = go 0

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
