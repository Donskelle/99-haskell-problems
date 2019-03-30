-- https://wiki.haskell.org/99_questions/21_to_28

-- λ> isPrime 7
-- True
isPrime :: Int -> Bool
isPrime number = length [y | y <- [2..number], mod number y  == 0 ] == 1

-- λ> [myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6]
-- [9,3,3]
-- myGCD :: Int -> Int -> Int

-- λ> coprime 35 64
-- True


-- λ> totient 10
-- 4


-- λ> primeFactors 315
-- -- [3, 3, 5, 7]


-- λ> prime_factors_mult 315
-- [(3,2),(5,1),(7,1)]

