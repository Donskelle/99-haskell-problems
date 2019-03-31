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
        encodeInternal (x:xs) accu = encodeInternal xs $ add x accu
          where add char [] = [Single char]
                add char ls
                  | containsEle (last ls) char = init ls ++ [increaseList $ last ls ]
                  | otherwise = ls ++ [Single char]

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

addToLast :: [ListItem a] -> [ListItem a]
addToLast ls = init ls ++ [increaseList $ last ls] 

encodeModified' :: (Eq a) => [a] -> [ListItem a]
encodeModified' [] = []
encodeModified' list = foldl (increaseList') [] list

increaseList' :: (Eq a) => [ListItem a] -> a -> [ListItem a]
increaseList' [] char = [Single char]
increaseList' acc char
  | containsEle (last acc) char = addToLast acc
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
dropEvery list count = map fst $ filter ((\x -> x `mod` count /= 0).snd) $ zip list [1..]


-- λ> split "abcdefghik" 3
-- ("abc", "defghik")
split :: [a] -> Int -> ([a], [a])
split list count = (take count list, drop count list)
split' list count = splitAt count list


-- λ> slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
-- "cdefg"
slice :: [a] -> Int -> Int -> [a]
slice list start end = take (end - start + 1) $ drop (start-1) list


-- λ> rotate ['a','b','c','d','e','f','g','h'] 3
-- "defghabc"
-- λ> rotate ['a','b','c','d','e','f','g','h'] (-2)
-- "ghabcdef"
-- rotate :: [a] -> Int -> [a]
rotate :: [a] -> Int -> [a]
rotate list count = drop count list ++ take count list
rotate' [] count = error "empty list"
rotate' (x:xs) count
  | count == 0 = x:xs
  | count < 0 = rotate' ((last (x:xs)):(init (x:xs))) $ count+1
  | otherwise = rotate' (xs++[x]) (count - 1)


-- λ> removeAt 2 "abcd"
-- ('b',"acd")
removeAt :: Int -> [a] ->  (a, [a])
removeAt pos list
  | pos > length list = error "Int out of range"
  | otherwise = (list!!arrPos, let (ys,zs) = splitAt arrPos list in ys ++ tail zs)
    where arrPos = pos - 1