import           Test.QuickCheck
import           Primes
import           Data.Maybe
import           Data.Either

getDivisors :: Int -> [Int]
getDivisors n = filter ((== 0) . (n `mod`)) [2 .. (n - 1)]

main :: IO ()
main = do
    quickCheck prop_validPrimesOnly
    quickCheckWith stdArgs { maxSuccess = 100000 } prop_primesArePrime
    quickCheckWith stdArgs { maxSuccess = 100000 } prop_nonPrimesAreComposite
    quickCheck prop_factorsMakeOriginal
    quickCheck prop_allFactorsPrime
    quickCheckWith stdArgs { maxSuccess = 100000 } prop_HMNonPrimesAreComposite
    quickCheckWith stdArgs { maxSuccess = 100000 } prop_HMPrimesArePrime


prop_validPrimesOnly val = if val < 2 || val >= length primes
    then isLeft result
    else True
    where result = isPrime val

prop_primesArePrime val = if result == Right True
    then length divisors == 0
    else True
  where
    result   = isPrime val
    divisors = getDivisors val

prop_nonPrimesAreComposite val = if result == Right False
    then length divisors > 0
    else True
  where
    result   = isPrime val
    divisors = getDivisors val

prop_factorsMakeOriginal val = if isLeft result
    then True
    else (product (fromRight ([1] :: [Int]) result)) == val
    where result = primeFactors val

prop_allFactorsPrime val = if isLeft result
    then True
    else all (== Right True) resultsPrime
  where
    result       = primeFactors val
    resultsPrime = map isPrime (fromRight [2] result)


prop_HMNonPrimesAreComposite val witnesses = if result == Right False
    then length divisors > 0
    else True
  where
    result   = millerRabin val witnesses
    divisors = getDivisors val

prop_HMPrimesArePrime val witnesses =
    if result == Right True && length witnesses > 1000
        then length divisors == 0
        else True
  where
    result   = millerRabin val witnesses
    divisors = getDivisors val
