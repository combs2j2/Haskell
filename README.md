# Haskell

My primary academic background being mathematics, I have found Haskell to be an exceedingly useful tool for finding solutions to problems in a number of fields, especially in Number Theory.

primes.hs implements a number of functions involving prime numbers and residues. isPrime :: Int -> Bool determines if a positive integer is prime, specifically by determining if the given input has any primes factors below its square root; if a number has no prime factors below its square root, then the number has no prime factors at all, thus the number is prime. numPrimesUpTo :: Int -> Int determines the number of prime numbers between 0 and the given input. This is calculated recursively, with the base case being n = 2, which evaluates to 1 since 2 is prime, but 0 and 1 are not. densityOfPrimes :: Int -> Rational determines the proportion of numbers from 1 up to the given input that are prime. The function calls numPrimesUpTo n, then divides this result by n. Both numPrimesUpTo n and n are converted to Rationals, then these two rationals are divided to produce the desired (fraction) output. relativelyPrime :: Int -> Int -> Bool determines if the two integer inputs are relatively prime. That is, if the inputs have a gcd (greatest common divisor) of 1. eulerPhi :: Int -> Int calculates the number of integers between 1 and the input that are relatively prime to the input. eulerPhi n generates a list of integers from 1 to n that are relatively prime to n and calculates the length of the list, and therefore the output of the function.   

collatz.hs studies the infamous Collatz Conjecture, which is as follows: Take any natural number (1,2,3,...). If the number is odd, multiply it by 3 and add 1. If it is even, simply divide by 2. Repeat the same process for the result. The conjecture states that no matter the input, this procedure always reaches 1, and upon repeating the process produces 1->4->2->1->4->2->1->... That is, every number passed through the procedure reaches a "1->4->2->1" loop. Nobody has been able to prove this seemingly elementary conjecture for all natural numbers, but it has been verified for every number less than 2^68. This file implements stepsTo1, which determines the number of steps it takes for the input value to reach 1. The helper function collatzHelp is implemented tail recursively with an accumulator input (a) and the number we are anlyzing the conjecture for (n). In collatzHelp a n, if n = 1, then we return our accumulator a. Otherwise, if n is divisible by 2, we apply collatzHelp (a+1) (n/2), and if n is not divisible by 2, we apply collatzHelp (a+1) (3n+1). stepsTo1 simply initializes a to be 0. collatzFirst n is a list containing the number of steps it takes for each number to reach 1 from 1 up to n. 
