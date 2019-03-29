-- https://wiki.haskell.org/H-99:_Ninety-Nine_Haskell_Problems
myLast :: [a] -> a
myLast [] = error "Empty list"
myLast a = last a

myButLast :: [a] -> a
myButLast [] = error "Empty list"
myButLast [x] = error "Only one ele"
myButLast a = last (init a)

elementAt :: [a] -> Int -> a
elementAt list i = list !! (i)

myLength :: [a] -> Int
myLength list = length list
myLength' [] = 0
myLength' (x:xs) = 1 + myLength' xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]


-- λ> isPalindrome "madamimadam"
-- False
isPalindrome :: Eq a => [a] -> Bool
isPalindrome list = list == (myReverse list)


-- * (my-flatten '(a (b (c d) e)))
-- (A B C D E)
data NestedList a = Elem a | List [NestedList a]
myFlatten :: NestedList a -> [a]
myFlatten (Elem a) = [a]
myFlatten (List []) = []
myFlatten (List (x:xs)) = myFlatten x ++ myFlatten (List xs)


-- (A B C A D E)
-- * (compress '(a a a a b c c a a d e e e e))
compress :: Eq a => [a] -> [a]
compress [] = []
compress list = compressInternal list []
  where compressInternal [] accu = accu
        compressInternal [x] accu = accu ++ [x]
        compressInternal (x:xs) accu
          | x == (head xs) = compressInternal xs accu
          | otherwise = compressInternal xs (accu ++ [x])


-- * (pack '(a a a a b c c a a d e e e e))
-- ((A A A A) (B) (C C) (A A) (D) (E E E E))
pack :: (Eq a) => [a] -> [[a]]
pack list = packInternal list []
  where packInternal [] accu = accu
        packInternal (x:xs) accu = packInternal xs (addPack x accu)
          where addPack a [] = [[a]]
                addPack a (x:xs)
                  | head x == a = [x ++ [a]] ++ xs
                  | otherwise = [x] ++ (addPack a xs)


-- (encode '(a a a a b c c a a d e e e e))
-- ((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))
encode :: (Eq a) => [a] -> [(Int, a)]
encode list = packInternal list []
  where packInternal [] accu = accu
        packInternal (x:xs) accu = packInternal xs (addPack x accu)
          where addPack a [] = [(1, a)]
                addPack a (x:xs)
                  | snd x == a = [(fst x + 1, snd x)] ++ xs
                  | otherwise = [x] ++ (addPack a xs)