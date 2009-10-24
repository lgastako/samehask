module Play where

--my_replicate :: (Num i, Ord i) => i -> a -> [a]
my_replicate :: (Num a, Ord a, Enum a) => a -> b -> [b]
my_replicate n x
           | n <= 0    = []
           | otherwise = x:my_replicate (pred n) x


my_take :: (Num a, Ord a, Enum a) => a -> [b] -> [b]
my_take _ [] = []
my_take n (x:xs)
      | n <= 0 = []
      | otherwise = x:my_take (pred n) xs


my_rev :: [a] -> [a]
my_rev [] = []
my_rev (x:xs) = my_rev xs ++ [x]


my_repeat :: a -> [a]
my_repeat x = x:my_repeat x


my_zip :: [a] -> [b] -> [(a,b)]
my_zip [] _ = []
my_zip _ [] = []
my_zip (x:xs) (y:ys) = (x, y):my_zip xs ys


my_elem :: (Eq a) => a -> [a] -> Bool
my_elem _ [] = False
my_elem y (x:xs)
      | x == y    = True
      | otherwise = my_elem y xs


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (pivot:xs) = smallerSorted ++ [pivot] ++ largerSorted
                 where smallerSorted = quicksort [x | x <- xs, x <= pivot]
                       largerSorted  = quicksort [x | x <- xs, x > pivot]


my_zip_with :: (a -> b -> c) -> [a] -> [b] -> [c]
my_zip_with _ [] _          = []
my_zip_with _ _ []          = []
my_zip_with f (x:xs) (y:ys) = f x y : my_zip_with f xs ys


my_flip :: (a -> b -> c) -> (b -> a -> c)
my_flip f y x = f x y


my_map :: (a -> b) -> [a] -> [b]
my_map _ []     = []
my_map f (x:xs) = f x : my_map f xs


my_filter :: (a -> Bool) -> [a] -> [a]
my_filter _ [] = []
my_filter f (x:xs)
   | f x       = x : my_filter f xs
   | otherwise = my_filter f xs


sum_odd_lt_10k :: Integer
sum_odd_lt_10k = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))


collatz_chain :: (Integral a) => a -> [a]
collatz_chain 1 = [1]
collatz_chain n
       | even n    =  n:collatz_chain (n `div` 2)
       | otherwise =  n:collatz_chain (n * 3 + 1)


-- for all starting numbers between 1 and 100, how many chains have a
-- length greater than 15?
how_many_collatz_1_to_100_gt_15 :: Int
how_many_collatz_1_to_100_gt_15 =
    length (filter (>15) (map length (map collatz_chain [1..100])))


my_fold :: (a -> b -> a) -> a -> [b] -> a
my_fold f y []     = y
my_fold f y (x:xs) = my_fold f (f y x) xs


my_foldl1 :: (a -> a -> a) -> [a] -> a
my_foldl1 f (x:xs) = my_fold f x xs


-- write this in point free style:
-- fn x = ceiling (negate (tan (cos (max 50 x))))  

my_fn :: Double -> Integer
my_fn = ceiling . negate . tan . cos . max 50