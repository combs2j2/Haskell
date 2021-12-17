collatzHelp :: Integer -> Integer -> Integer
collatzHelp a 1 = a
collatzHelp a n = if (n `mod` 2) == 0 then collatzHelp (a+1) (n `div` 2) else collatzHelp (a+1) (3*n + 1)

stepsTo1 = collatzHelp 0

collatzFirstN n = if n >= 1 then [stepsTo1 x | x <- [1..n]] else error "n must be at least 1"