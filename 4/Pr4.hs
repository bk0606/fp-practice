
deleteIf :: (a -> Bool) -> [a] -> [a]
deleteIf f [] = []
deleteIf f (x:xs) = if f x
                    then deleteIf f xs
                    else x : deleteIf f xs

deleteEvens = filter odd

squaresOf :: [Int] -> [Int]
squaresOf [] = []
squaresOf (x:xs) = x*x : squaresOf xs

isEvens :: [Int] -> [Bool]
isEvens [] = []
isEvens (x:xs) = even x : isEvens xs

myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (x:xs) = f x : myMap f xs

squaresOf' = myMap (\x -> x*x)
isEvens' = myMap even

mySum :: [Int] -> Int
mySum [] = 0
mySum (x:xs) = x + mySum xs

-- [1,2,3,4] -> [[1],[2],[3],[4]]
myFun :: [Int] -> [[Int]]
myFun [] = []
myFun (x:xs) = [x] : myFun xs

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f b [] = b
myFoldr f b (x:xs) = f x $ myFoldr f b xs

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f a [] = a
myFoldl f a (x:xs) = myFoldl f (f a x) xs

myFun' = myFoldr (\x res -> [x] : res) []
myFun'' = myFoldl (\res x -> [x] : res) []

-- foldl1. foldr1

myMaximum :: (Ord a) => [a] -> a
myMaximum = foldr1 (\a b -> if a > b then a else b)

g :: (Integer, Char)
g = (1,'a')

-- fst, snd

f :: (Integer, Char, [Int])
f = (1, 'b', [1,3,4])
            
