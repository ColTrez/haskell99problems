import System.Random
import Data.List

-- problem 1
myLast :: [a] -> a
myLast (x:[]) = x
myLast (x:xs) = myLast xs

-- problem 2
myButLast :: [a] -> a
myButLast xs = head $ tail $ take 2 $ reverse xs

--better solution
myButLast' :: Foldable f => f a -> Maybe a
myButLast' = fst . foldl (\(a,b) x -> (b, Just x)) (Nothing,Nothing)

-- problem 3
problem3Lazy :: Int -> [a] -> a
problem3Lazy x = (\as -> as !! (x-1))

nthElement :: Int -> [a] -> a
nthElement 1 (x:xs) = x
nthElement n (x:xs) = nthElement (n - 1) xs

--problem 4
myLength :: [a] -> Int
myLength = foldl (\x _ -> x + 1) 0

--problem 5
myReverse :: [a] -> [a]
myReverse = foldl (\x y -> [y] ++ x) []

--problem 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = reverse xs == xs

--problem 7
data NestedList a = Elem a | List[NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
--flatten (List[Elem a, NestedList b]) = a : 
flatten (List x) = concatMap flatten x

--problem 8
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs) = (x : compress (dropWhile ( == x) xs))

--problem 9
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x : takeWhile (== x) xs) : pack (dropWhile ( == x) xs)

--problem 10
runLengthEncode :: Eq a => [a] -> [(Int,a)]
runLengthEncode xs = [(length b, head b) | b <- pack xs]

--problem 11
data ModifiedEncoding a = Single a | Multiple (Int, a)
                    deriving Show

encodeModified :: Eq a => [a] -> [ModifiedEncoding a]
encodeModified = map modify . runLengthEncode
            where
                modify :: Eq a => (Int,a) -> ModifiedEncoding a
                modify (1, x) = Single x
                modify (n, x) = Multiple (n, x)

--problem 12
decodeModified :: [ModifiedEncoding a] -> [a]
decodeModified [] = []
decodeModified (x:xs) = decode x ++ decodeModified xs
                    where
                        decode :: ModifiedEncoding a -> [a]
                        decode (Single a) = [a]
                        decode (Multiple (n, a)) = take n (repeat a)

--problem 13
--encodeDirect :: [a] -> [ModifiedEncoding a]
--encodeDirect [] = []
    {-encodeDirect :: Eq a => [a] -> [ModifiedEncoding a] 
encodeDirect xs = (base:(recurseOn base xs))
                where
                   --build :: Eq a => a -> Int -> ModifiedEncoding a
                   build a 0 = Single a
                   build a n = Multiple (n+1, a)

                   --count :: Eq a => a -> [a] -> Int
                   count _ [] = 0
                   count a (x:xs) = if a /= x then 0 else 1 + count a xs

                   --encodeOne :: Eq a => [a] -> ModifiedEncoding a 
                   encodeOne (x:xs) = build x $ count x xs
                   
                   --base :: Eq a => ModifiedEncoding a
                   base = encodeOne xs
                   
                   --recurseOn :: Eq a => ModifiedEncoding a -> [a] -> ModifiedEncoding a
                   recurseOn (Single a) xs = encodeOne . tail xs
                   recurseOn (Multiple (n, a)) xs = encodeOne . drop n xs
-}

--problem 14
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x : [x] ++ (dupli xs)

--problem 15
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = (take n $ repeat x) ++ repli xs n

--problem 16
dropN :: [a] -> Int -> [a]
dropN xs n = map fst $ filter (\p -> snd p `mod` n /= 0) $ zip xs [1..] 
                   

--problem 17
splitN :: [a] -> Int -> ([a],[a])
--first solution is too ez
--splitN xs n = (take n xs, drop n xs)
splitN [] _ = ([], [])
splitN xs 0 = ([], xs)
splitN (x:xs) 1 = ([x], xs)
splitN (x:xs) n = let recurse = splitN xs (n-1) in ([x] ++ fst recurse, snd recurse)

--problem 18
sliceNM :: [a] -> Int -> Int -> [a]
sliceNM xs n m = map fst $ filter (\(a,b) -> b >= n && b <= m) $ zip xs [1..]

--problem 19
rotateN :: [a] -> Int -> [a]
rotateN [] _ = []
rotateN xs n | n < 0     = rotateN xs (length xs + n)
             | otherwise = drop n xs ++ take n xs

--problem 20
removeAtX :: [a] -> Int -> [a]
    {-- this solution is trivial
removeAtX [] _ = []
removeAtX as x = take (x-1) as ++ drop x as
--}
removeAtX [] _ = []
removeAtX _ 0 = []
removeAtX (x:xs) 1 = xs
removeAtX (x:xs) n = x : (removeAtX xs (n-1))

--problem 21
insertAt :: Int -> a -> [a] ->[a]
insertAt _ a [] = [a]
insertAt n a (x:xs) | n <= 1    = a:(x:xs)
                    | otherwise = x : (insertAt (n-1) a xs)

--problem 22
rangeBetween :: Int -> Int -> [Int]
--trivial solution for the meme
--rangeBetween a b = [a..b]
--less trivial
rangeBetween a b | a == b    = [a]
                 | a > b     = [b]
                 | otherwise = a : rangeBetween (succ a) b

--problem 25
shuffle :: [a] -> StdGen -> [a]
shuffle as gen = let n = length as
                     rands = take n $ randoms gen :: [Int]
                     pairs = zip as rands
                  in map fst $ sortOn snd pairs 

--rndPerm :: [a] -> StdGen -> [a]
--rndPerm xs gen = 
