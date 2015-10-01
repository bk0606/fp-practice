
-- Бесконечный список из единиц
inf1 :: [Int]
inf1 = 1 : inf1
       
-- zipWith
zipW :: (a -> b -> c) -> [a] -> [b] -> [c]
zipW f [] [] = []
zipW f (a:as) (b:bs) = f a b : zipW f as bs
                       
-- Числа Фибоначчи
fib = 1:1:zipWith (+) fib (tail fib)
      
-- Натуральные числа (начинаем с 1, хотя можно поспорить)
-- nat = [1..]
nat = 1 : map (+1) nat

-- Проверка на простоту (почти в лоб)
isPrime 1 = False
isPrime 2 = True
isPrime n | even n = False
          | otherwise = null $ filter (\i -> n`mod`i == 0) $ takeWhile (\i -> i*i<=n) [3,5..]

-- Простые числа -- подмножество натуральных такое, что каждый элемент прост
primes = filter isPrime nat

-- zip = zipWith (\a b -> (a,b))
isTwin (a,b) = b-a == 2
twins = filter isTwin $ zip primes $ tail primes

