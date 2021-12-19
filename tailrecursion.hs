factorial :: Integer -> Integer
factorial 1 = 1
factorial 0 = 1
factorial n = n * factorial (n-1)

factHelp :: Integer -> Integer -> Integer
factHelp a 1 = a
factHelp a n = factHelp (a*n) (n-1)

factorialTR = factHelp 1

fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

fibHelp :: Integer -> Integer -> Integer -> Integer
fibHelp a b 0 = a
fibHelp a b n = fibHelp b (a+b) (n-1)

fibTR = fibHelp 0 1