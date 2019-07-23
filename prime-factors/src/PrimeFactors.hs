module PrimeFactors (primeFactors) where

primeFactors :: Integer -> [Integer]
primeFactors 1 = []
primeFactors x = prime x 2
  where 
    prime :: Integer -> Integer -> [Integer]
    prime n curr 
      | curr * curr > n = [n]
      | modN == 0       = curr : primeFactors divN
      | otherwise       = prime n (curr + 1)
      where (divN, modN) = divMod n curr

-- Some interesting community solution
primeFactors' :: Integer -> [Integer]
primeFactors' = go possiblePrimes
 where
  go _ 1 = []
  go (x:xs) n
    | x * x > n     = [n]
    | x `divides` n = x : go (x:xs) (n `div` x)
    | otherwise     = go xs n

  divides x y = y `rem` x == 0
  possiblePrimes = 2 : 3 : [f n | n <- [6, 12..], f <- [pred, succ]]

