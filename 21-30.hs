-- https://wiki.haskell.org/99_questions/21_to_28

-- λ> insertAt 'X' "abcd" 2
-- "aXbcd"


-- λ> range 4 9
-- [4,5,6,7,8,9]


-- λ> rnd_select "abcdefgh" 3 >>= putStrLn
-- eda


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

