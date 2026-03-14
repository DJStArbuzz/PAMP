sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

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
