multThree :: Int -> Int -> Int -> Int
multThree x y z = x * y * z

compareWithHundred :: Int -> Ordering
compareWithHundred x = compare 100 x

compareWithHundred' :: Int -> Ordering
compareWithHundred' = compare 100

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

subtractFour :: Int -> Int
subtractFour = subtract 4

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
    | p x       = x : filter' p xs
    | otherwise = filter' p xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerOrEqual = filter (<= x) xs
        lager = filter (> x) xs
    in  quicksort smallerOrEqual ++ [x] ++ quicksort lager

largestDivisible :: Integer
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0 -- 「foldl (+) 0」はリストを取る関数を返すため、引数のxsは省略出来る

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldr (\x acc -> if x == y then True else acc) False ys

maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 max

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) []

and' :: [Bool] -> Bool
and' = foldr (&&) True

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000)  (scanl1 (+) (map sqrt [1..]))) + 1

