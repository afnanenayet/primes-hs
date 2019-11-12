module Primes
    ( primes
    , sieve
    , isPrime
    , primeFactors
    , millerRabin
    )
where

import           System.Random
import           Control.Monad                  ( replicateM )

-- | An error type describing the possible errors that could arise from the
-- prime number routines
data PrimeError = InputTooSmall
                | InputTooBig
                | InvalidInput
                | NotEnoughWitnesses deriving (Show, Eq)

maxPrime :: Int
maxPrime = 10000

-- |A prime number generator
primes :: [Int]
primes = sieve [2 .. maxPrime]

-- |The sieve of Eratosthenes technique for generating prime numbers
sieve :: [Int] -> [Int]
sieve []                 = []
sieve (nextPrime : rest) = nextPrime : sieve noFactors
    where noFactors = filter ((/= 0) . (`mod` nextPrime)) rest

-- |Returns whether a valid input is a prime number. The number must be
-- positive and less than the max value.
isPrime :: Int -> Either PrimeError Bool
isPrime n | n < 2              = Left InputTooSmall
          | n >= length primes = Left InputTooBig
          | otherwise          = Right $ n `elem` primes

-- |Returns the prime factorization of a number as a list of integers. This
-- will return `Nothing` for numbers that can't be factorized by this method
-- (e.g. negative numbers, numbers greater than the max integer).
primeFactors :: Int -> Either PrimeError [Int]
primeFactors n | n < 2              = Left InputTooSmall
               | n >= length primes = Left InputTooBig
               | otherwise          = Right (primeFactors' n primesLessThanN)
    where primesLessThanN = filter (<= n) primes


-- | The helper function for `primeFactors`. This assumes valid input (since
-- it's filtered out by the method this is helping). This filters a range of
-- factors by checking if a number is divisible by any number of primes.
primeFactors' :: Int -> [Int] -> [Int]
primeFactors' 0 []              = []
primeFactors' n []              = []
primeFactors' n (next : primes) = if n `mod` next == 0
    then next : primeFactors' (n `div` next) (next : primes)
    else primeFactors' n primes

-- | The monadic implementation of the Miller-Rabin primality test. This is a
-- thin wrapper for the `millerRabin` method which generates a list of random
-- numbers for the user based on the number of rounds supplied.
millerRabinM :: Int -> Int -> IO (Either PrimeError Bool)
millerRabinM candidate rounds = do
    let candidate' = candidate - 1
    witnesses <- randomList rounds (2, candidate')
    return $ millerRabin candidate (take rounds witnesses)

-- | Generate a random list of `n` numbers within the range (lower, upper)
randomList :: Int -> (Int, Int) -> IO [Int]
randomList 0 _                    = return []
randomList n range@(lower, upper) = do
    r  <- randomRIO (lower, upper)
    rs <- randomList (n - 1) range
    return (r : rs)


-- | An implementation of the Miller-Rabin primality test. This is a
-- non-deterministic test that determines with certainty if a number is
-- composite. If it returns `True`, that does not mean the number is
-- necessarily prime, but that there is a *chance* that it is prime.
--
-- `rounds` describes the number of rounds to run the test with. You can treat
-- this as a heuristic for accuracy: a higher number is more computationally
-- expensive, but it is more accurate (which means that if a number is
-- "probably" prime, the confidence increases with the number of rounds).
--
-- `witnesses` are usually randomly generated numbers, which are not provided
-- by this method to keep it pure.
millerRabin :: Int -> [Int] -> Either PrimeError Bool
millerRabin candidate witnesses | candidate <= 3       = Left InputTooSmall
                                | candidate > maxPrime = Left InputTooBig
                                | length witnesses < 1 = Left NotEnoughWitnesses
                                | otherwise            = Right isPrime
  where
    rounds  = length witnesses
    results = map (millerRabinRound candidate) witnesses
    isPrime = and results

-- | A round of the Miller-Rabin primality test, assuming valid input
millerRabinRound :: Int -> Int -> Bool
millerRabinRound candidate rand | even candidate = False
                                | x == 1 || x == candidate' = True
                                | otherwise = candidate' `elem` iterXs
  where
    candidate' = candidate - 1
    (d, r)     = convertNToForm candidate
    x          = (rand ^ d) `mod` candidate
    iterXs     = take (r - 1) $ iterate (`squareMod` candidate) r

-- | Convert the input `n` to the form $2^r * d + 1$, where d is odd. This method
-- returns $(r, d)$.
convertNToForm :: Int -> (Int, Int)
convertNToForm = convertNToForm' 0

convertNToForm' :: Int -> Int -> (Int, Int)
convertNToForm' r d | rem == 1  = (r, d)
                    | otherwise = convertNToForm' (r + 1) quot
    where (quot, rem) = quotRem d 2

-- | Square `x` and take the modulo against `a`
squareMod :: Int -> Int -> Int
squareMod x a = (x ^ 2) `rem` a
