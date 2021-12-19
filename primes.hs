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

primeDivisors :: Integer -> [Integer]
primeDivisors 1 = [1]
primeDivisors 0 = [1..]
primeDivisors n = if isPrime n then [n] else [x | x <- [1..(isqrt n)], divides x n, isPrime x]

divides :: Integer -> Integer -> Bool
divides x n = if n `mod` x == 0 then True else False

pfHelp :: [Integer] -> Integer -> [Integer]
pfHelp a 1 = []
pfHelp a 0 = error "infinitely many prime factorizations of 0"
pfHelp a n
	| isPrime n = a ++ [n]
	| (isPrime n == False) = pfHelp (a ++ [b]) (n `div` b) where
		b = head (primeDivisors n)

primeFactorization = pfHelp []
