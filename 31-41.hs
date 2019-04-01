-- https://wiki.haskell.org/99_questions/21_to_28

-- λ> isPrime 7
-- True
isPrime :: Int -> Bool
isPrime number = length [y | y <- [2..number], mod number y == 0 ] == 1

-- λ> [myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6]
-- [9,3,3]
myGCD :: Int -> Int -> Int
myGCD a b
  | b == 0 = a
  | otherwise = myGCD b $ a `mod` b


-- λ> coprime 35 64
-- True
coprime :: Int -> Int -> Bool
coprime a b
  | myGCD a b == 1 = True
  | otherwise = False

-- λ> totient 10
-- 4
totient :: Int -> Int
totient number = length [x | x <- [1..(number-1)], myGCD x number == 1]


createInfPrimes = [x | x <- [2..], isPrime x]

-- λ> primeFactors 315
-- -- [3, 3, 5, 7]
primeFactors :: Int -> [Int]
primeFactors number = findPrimeFactor number 0
  where 
    findPrimeFactor primeFac n
      | primeFac == 1 = []
      | primeFac `mod` createInfPrimes!!n == 0 = createInfPrimes!!n : findPrimeFactor (div primeFac (createInfPrimes!!n)) 0
      | otherwise = findPrimeFactor primeFac $ n + 1 


-- λ> prime_factors_mult 315
-- [(3,2),(5,1),(7,1)]

-- λ> primesR 10 20
-- [11,13,17,19]
primesR :: Int -> Int -> [Int]
primesR from to = [x | x <- [from..to], isPrime x]


-- λ> goldbach 28
-- (5, 23)
goldbach :: Int -> (Int, Int)
goldbach number = head [ (x,y) | x <- primesR 2 (number - 2), y <- primesR 2 (number - 2), x + y == number]


-- λ> goldbachList 9 20
-- [(3,7),(5,7),(3,11),(3,13),(5,13),(3,17)]
-- λ> goldbachList' 4 2000 50
-- [(73,919),(61,1321),(67,1789),(61,1867)]
goldbachList :: Int -> Int -> [(Int, Int)]
goldbachList from to = map goldbach [x | x <- [from..to], even x]