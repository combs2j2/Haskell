# Haskell

My primary academic background being mathematics, I have found Haskell to be an exceedingly useful tool for finding solutions to problems in a number of fields, especially in Number Theory.

primes.hs implements a number of functions involving prime numbers and residues. isPrime :: Int -> Bool determines if a positive integer is prime, specifically by determining if the given input has any primes factors below its square root; if a number has no prime factors below its square root, then the number has no prime factors at all, thus the number is prime. numPrimesUpTo :: Int -> Int determines the number of prime numbers between 0 and the given input. This is calculated recursively, with the base case being n = 2, which evaluates to 1 since 2 is prime, but 0 and 1 are not. densityOfPrimes :: Int -> Rational determines the proportion of numbers from 1 up to the given input that are prime. The function calls numPrimesUpTo n, then divides this result by n. Both numPrimesUpTo n and n are converted to Rationals, then these two rationals are divided to produce the desired (fraction) output. relativelyPrime :: Int -> Int -> Bool determines if the two integer inputs are relatively prime. That is, if the inputs have a gcd (greatest common divisor) of 1. eulerPhi :: Int -> Int calculates the number of integers between 1 and the input that are relatively prime to the input. eulerPhi n generates a list of integers from 1 to n that are relatively prime to n and calculates the length of the list, and therefore the output of the function.   

collatz.hs studies the infamous Collatz Conjecture, which is as follows: Take any natural number (1,2,3,...). If the number is odd, multiply it by 3 and add 1. If it is even, simply divide by 2. Repeat the same process for the result. The conjecture states that no matter the input, this procedure always reaches 1, and upon repeating the process produces 1->4->2->1->4->2->1->... That is, every number passed through the procedure reaches a "1->4->2->1" loop. Nobody has been able to prove this seemingly elementary conjecture for all natural numbers, but it has been verified for every number less than 2^68. This file implements stepsTo1, which determines the number of steps it takes for the input value to reach 1. The helper function collatzHelp is implemented tail recursively with an accumulator input (a) and the number we are anlyzing the conjecture for (n). In collatzHelp a n, if n = 1, then we return our accumulator a. Otherwise, if n is divisible by 2, we apply collatzHelp (a+1) (n/2), and if n is not divisible by 2, we apply collatzHelp (a+1) (3n+1). stepsTo1 simply initializes a to be 0. collatzFirst n is a list containing the number of steps it takes for each number to reach 1 from 1 up to n. 

listOps.hs is an implementation of basic list operations (append, concatenate, filter, length, map, folds, reverse). Tail recursive helper functions are utilized for myLength and myConcat to optimize efficiency.

tailrecursion.hs provides recursive definitions for n! (n factorial) and the nth fibonacci number as well as analogous tail recursive implementations.

MillerRabin.hs implements the Miller-Rabin primality test, a method of determining if a given integer is probably prime. The test is based on the mathematical fact that if p is a prime, gcd(a,p)=1, and p-1=(2^s)d where d is an odd integer, p divides at least one of the following: (a^((p-1)/2)+1), (a^((p-1)/4)+1), (a^((p-1)/8)+1),..., (a^d + 1), (a^d - 1). It is possible that a given composite number divides one of these factors, but the probability of this occuring for any given a is bounded above by 1/4. Thus, running k number of tests for randomly selected a's yields an accuracy of at least 1-(1/4)^k. The test has time complexity O(k * log^2(b)) where b is the number of bits in the input integer and k is the number of times the test is performed. Thus, when k is held constant (say k=40, for instance, in which the probability of a false positive is less than (1/4)^40), the algorithm has log^2 complexity. Thus, the algorithm is widely used for quickly generating primes that are hundreds of digits long, a necessity for secure and efficient cryptographic techniques.

Pollard.hs implements Pollard's p-1 method, a factorization algorithm that based on Fermat's Little Theorem: If p is prime and gcd(a,p)=1, then a^(p-1)=1 mod p. Thus if (p-1) divides some number m, then a^m=1 mod p. The algorithm works as follows: given an input integer n, choose a number a relatively prime to n. For k=1,2,..., compute gcd(a^(k!) - 1, n). If the result is 1, increase k and repeat. If the result is n, restart the algorithm with a different a. This method is suitable for factoring numbers whose prime factors p1, p2,..., pr have the following property: for each i, pi-1 is composed of only small prime factors. This is because for each pi, pi-1 may only divide k! when k is at least pi-1's largest factor.
