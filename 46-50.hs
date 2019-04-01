import Control.Monad (replicateM)

-- λ> table (\a b -> (and' a (or' a b)))f
-- True True True
-- True False True
-- False True False
-- False False False
and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False
or' ::  Bool -> Bool -> Bool
or' False False = False
or' _ _ = True
table :: (Bool->Bool->Bool) -> IO ()
table f = putStr $ unlines [show a ++ " " ++ show b ++ " " ++ show (f a b) | a<-[True, False], b<-[True, False]]


-- λ> table2 (\a b -> a `and'` (a `or'` not b))
-- True True True
-- True False True
-- False True False
-- False False False
table2 f = table f


-- λ> tablen 3 (\[a,b,c] -> a `and'` (b `or'` c) `equ'` a `and'` b `or'` a `and'` c)
-- -- infixl 3 `equ'`
-- True  True  True  True
-- True  True  False True
-- True  False True  True
-- True  False False True
-- False True  True  True
-- False True  False True
-- False False True  True
-- False False False True
equ' :: Bool -> Bool -> Bool 
equ' True True = True
equ' False False = True
equ' _ _ = False
tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen number f = putStr $ unlines [showArr a ++ " " ++ show (f a) | a <- genList number]
  where showArr = unwords . map(\x-> show x ++ " ")
        genList n = replicateM n [True, False]


-- -- infixl 7 `equ'`
-- True  True  True  True
-- True  True  False True
-- True  False True  True
-- True  False False False
-- False True  True  False
-- False True  False False
-- False False True  False
-- False False False False


-- λ> gray 3
-- ["000","001","011","010","110","111","101","100"]
-- gray :: Int -> [[String]]
gray n = map (\x -> foldl1 (++) $ map show x) $ replicateM n [0,1]


-- λ> huffman [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]
-- [('a',"0"),('b',"101"),('c',"100"),('d',"111"),('e',"1101"),('f',"1100")]
