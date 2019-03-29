-- https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems

-- * (encode-modified '(a a a a b c c a a d e e e e))
-- ((4 A) B (2 C) (2 A) D (4 E))


-- λ> decodeModified 
--        [Multiple 4 'a',Single 'b',Multiple 2 'c',
--         Multiple 2 'a',Single 'd',Multiple 4 'e']
-- "aaaabccaadeeee"


-- * (encode-direct '(a a a a b c c a a d e e e e))
-- ((4 A) B (2 C) (2 A) D (4 E))
-- λ> encodeDirect "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',
--  Multiple 2 'a',Single 'd',Multiple 4 'e']


-- λ> dupli [1, 2, 3]
-- [1,1,2,2,3,3]


-- λ> repli "abc" 3
-- "aaabbbccc"


-- λ> dropEvery "abcdefghik" 3
-- "abdeghk"


-- λ> split "abcdefghik" 3
-- ("abc", "defghik")


-- λ> slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
-- "cdefg"


-- λ> rotate ['a','b','c','d','e','f','g','h'] 3
-- "defghabc"
-- λ> rotate ['a','b','c','d','e','f','g','h'] (-2)
-- "ghabcdef"


-- λ> removeAt 2 "abcd"
-- ('b',"acd")