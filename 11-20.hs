-- https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems

-- λ> encodeModified "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',
--  Multiple 2 'a',Single 'd',Multiple 4 'e']
data ListItem a = Single a | Multiple Int a
  deriving(Show)
encodeModified :: (Eq a) => [a] -> [ListItem a]
encodeModified [] = []
encodeModified list = encodeInternal list []
  where encodeInternal [] accu = accu
        encodeInternal (x:xs) accu = encodeInternal xs (add x accu)
          where add char [] = [Single char]
                add char (y:ys)
                  | containsEle y char = [increaseList y] ++ ys
                  | otherwise = [y] ++ (add char ys)

containsEle :: (Eq a) => ListItem a -> a -> Bool
containsEle (Multiple _ a) b 
  | a == b = True
  | otherwise = False
containsEle (Single a) b
  | a == b = True
  | otherwise = False

increaseList :: ListItem a -> ListItem a
increaseList (Multiple count char) = Multiple (count+1) char
increaseList (Single char) = Multiple 2 char

addToHead :: [ListItem a] -> [ListItem a]
addToHead (x:xs) = xs ++  [increaseList x] 

encodeModified' :: (Eq a) => [a] -> [ListItem a]
encodeModified' [] = []
encodeModified' list = foldl (increaseList') [] list

increaseList' :: (Eq a) => [ListItem a] -> a -> [ListItem a]
increaseList' [] char = [Single char]
increaseList' acc char
  | containsEle (head acc) char = addToHead acc
  | otherwise = acc ++ [Single char]


-- λ> decodeModified 
--        [Multiple 4 'a',Single 'b',Multiple 2 'c',
--         Multiple 2 'a',Single 'd',Multiple 4 'e']
-- "aaaabccaadeeee"
decodeModified :: (Eq a) => [ListItem a] -> [a]
decodeModified [] = []
decodeModified list = foldl (convertToString) [] list
  where convertToString accu (Multiple count char) = accu ++ take count (repeat char)
        convertToString accu (Single char) = accu ++ [char]

-- * (encode-direct '(a a a a b c c a a d e e e e))
-- ((4 A) B (2 C) (2 A) D (4 E))
-- λ> encodeDirect "aaaabccaadeeee"
-- [Multiple 4 'a',Single 'b',Multiple 2 'c',
--  Multiple 2 'a',Single 'd',Multiple 4 'e']


-- λ> dupli [1, 2, 3]
-- [1,1,2,2,3,3]
dupli :: [a] -> [a]
dupli [] = []
dupli list = foldl (\accu x -> accu ++ [x] ++ [x] ) [] list

-- λ> repli "abc" 3
-- "aaabbbccc"
repli :: [a] -> Int -> [a]
repli list repliCount = foldl(\accu x -> accu ++ take repliCount (repeat x)) [] list

-- λ> dropEvery "abcdefghik" 3
-- "abdeghk"
dropEvery :: [a] -> Int -> [a]
dropEvery list count = map (snd) ((filter (\x -> x mod count /= 0).fst) (zip [1..] list))

-- λ> split "abcdefghik" 3
-- ("abc", "defghik")
-- split :: [a] -> Int -> [a]


-- λ> slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
-- "cdefg"
-- slice :: (Int b) => [a] -> b -> b -> [a]


-- λ> rotate ['a','b','c','d','e','f','g','h'] 3
-- "defghabc"
-- λ> rotate ['a','b','c','d','e','f','g','h'] (-2)
-- "ghabcdef"
-- rotate :: [a] -> Int -> [a]


-- λ> removeAt 2 "abcd"
-- ('b',"acd")
-- removeAt :: Int -> [a]