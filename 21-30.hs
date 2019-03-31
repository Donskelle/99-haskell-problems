-- https://wiki.haskell.org/99_questions/21_to_28

-- 
-- import random
-- import List

-- λ> insertAt 'X' "abcd" 2
-- "aXbcd"
insertAt :: a -> [a] -> Int -> [a]
insertAt char list pos = let (xs, ys) = splitAt pos list in xs ++ [char] ++ ys

-- λ> range 4 9
-- [4,5,6,7,8,9]
range :: Int -> Int -> [Int]
range from to = [from..to] 
range' from to = 
  if from > to then []
  else from : range' (from + 1) to 

-- λ> rnd_select "abcdefgh" 3 >>= putStrLn
-- eda
-- rnd_select :: [a] -> Int -> 

-- λ> diff_select 6 49
-- [23,1,17,33,21,37]


-- λ> rnd_permu "abcdef"
-- "badcef"


-- λ> combinations 3 "abcdef"
-- ["abc","abd","abe",...]


-- λ> group [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
-- [[["aldo","beat"],["carla","david","evi"],["flip","gary","hugo","ida"]],...]
-- (altogether 1260 solutions)
-- λ> group [2,2,5] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
-- [[["aldo","beat"],["carla","david"],["evi","flip","gary","hugo","ida"]],...]
-- (altogether 756 solutions)


-- λ> lsort ["abc","de","fgh","de","ijkl","mn","o"]
-- ["o","de","de","mn","abc","fgh","ijkl
-- lsort :: (Eq a) => [[a]] -> [[a]]
-- lsort = sortBy (\xs ys -> compare (length xs) (length ys))
