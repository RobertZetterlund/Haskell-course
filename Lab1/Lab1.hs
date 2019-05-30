import Test.QuickCheck
-- Power function from lecture
power :: Integer -> Integer -> Integer
power n k | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)

-- Part 1
steps :: Integer -> Integer
steps k
  | k == 0 = 1
  | k > 0  = 1 + steps(k-1)
  | otherwise = error "steps: negative argument"

-- Part 1 Test
prop_steps :: Integer -> Bool
prop_steps k = steps (abs k) == (abs k+1)

-- Part 2
power1 :: Integer -> Integer -> Integer
power1 n k
  | k < 0 = error "power1: negative argument"
  | otherwise = product [ n | _ <- [1..k]]

-- Part 2 Test
prop_power1 :: Integer -> Integer -> Bool
prop_power1 n k = power1 n k == n^k

-- Part 3
power2 :: Integer -> Integer -> Integer
power2 n k
  | k < 0 = error "power2: negative argument"
  | k == 0 = 1
  | even k = power2 (n*n) (div k 2)
  | odd k  = n * power2 n (k-1)

-- Part 3 Test
prop_power2 :: Integer -> Integer -> Bool
prop_power2 n k = power2 n k == n^k

-- Part 4

-- A
{-
What to test: Power functions (n^k)
Prerequisites (specified in assignment):
1. n is an integer
2. k is a positive integer (k >= 0)

Test design:
Because n is an integer, test both negative and positive values. Make sure to test n = 0 as well, as this could be considered an edge case.

Because k is a positive integer, test for some positive integers (including 0).

How to test:
1. Test edge cases (k = 0, n = 0)
2. Test for some positive Integers (n >= 0, k >= 0), both even and odd Integers.
3. Test for some negative Integers (n < 0), both even and odd Integers.
-}

-- B
prop_powers :: Integer -> Integer -> Bool
prop_powers n k = (power n k == power1 n k) && (power1 n k == power2 n k)

-- C
nList = [(-10), (-9) .. 10]
kList = [0, 1 .. 10]

-- Create list of tuples of possible combinations
-- examplePair :: (Integer, Integer)
possibleTests = [ (n,k) | n <- nList, k <- kList]

testPowers :: [(Integer, Integer)] -> Bool
testPowers [(a,b)] = prop_powers a b
testPowers (x:xs) = testPowers [x] && testPowers xs

-- D
-- k can only be a positive integer, therefor if a negative exponent is
-- passed as argument, take the absoulute value instead.
prop_powers' :: Integer -> Integer -> Bool
prop_powers' n k = prop_powers n (abs k)