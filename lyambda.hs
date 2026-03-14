drop' :: Int -> [a] -> [a]
drop' n xs = foldr(\x r k -> if k <= 0 then x : r (k - 1) else r (k - 1)) (const []) xs n

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p xs = foldr (\x r flag -> if flag && p x then r flag else x : r False) (const []) xs True

reverse' :: [a] -> [a]
reverse' = foldl(flip (:)) []

product' :: (Num a) => [a] -> a 
product' = foldl (*) 1

mapNfilter :: (a -> b) -> (b -> Bool) -> [a] -> [b] 
mapNfilter f p xs = foldr
  (\x acc -> if p (f x) then f x:acc else acc)
  [] xs

filterNmap :: (a -> Bool) -> (a -> b) -> [a] -> [b]
filterNmap p f xs = foldr
  (\x acc -> if p x then f x:acc else acc)
  [] xs

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr(\x acc -> f x:acc) [] xs

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldl(\acc x -> acc ++ [f x]) [] xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' x xs = foldl
  (\acc a -> if a == x then True else acc)
  False xs

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

flip' :: (a -> b -> c) -> b -> a -> c
flip' f = \x y -> f y x

multiplyThree :: Int -> Int -> Int -> Int
multiplyThree x y z = x * y * z

multiplyThree' :: Int -> Int -> Int -> Int
multiplyThree' = \x -> \y -> \z -> x * z * y

numCountChains :: Int
numCountChains = length (
                   filter (\xs -> length xs > 15)
                     (map chain [1..1000]))

chain :: Integer -> [Integer]
chain 1 = [1]
chain n
  | even n = n : chain (n `div` 2)
  | otherwise = n : chain (3 * n + 1)
