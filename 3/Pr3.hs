
data IL = N
        | C Int IL

g :: IL
g = C 3 (C 2 (C 1 N))

mySum :: IL -> Int
mySum N = 0
mySum (C x xs) = x + mySum xs

g' = [3,2,1]
mySum' :: [Int] -> Int
mySum' [] = 0
mySum' (x:xs) = x + mySum' xs

data List elem = Nil
               | Cons elem (List elem)
                 deriving (Show)

g2 :: List Int
g2 = Cons 3 $ Cons 2 $ Cons 1 Nil

mySum2 :: List Int -> Int
mySum2 Nil = 0
mySum2 (Cons x xs) = x + mySum2 xs

myLength :: List a -> Int
myLength Nil = 0
myLength (Cons _ xs) = 1 + myLength xs

deleteEvens :: List Int -> List Int
deleteEvens Nil = Nil
deleteEvens (Cons x xs) | even x    = deleteEvens xs
                        | otherwise = Cons x $ deleteEvens xs

deleteMod3 :: List Int -> List Int
deleteMod3 Nil = Nil
deleteMod3 (Cons x xs) | x`mod`3 == 0 = deleteMod3 xs
                       | otherwise    = Cons x $ deleteMod3 xs

deleteIf :: (Int -> Bool) -> List Int -> List Int
deleteIf f Nil = Nil
deleteIf f (Cons x xs) | f x = deleteIf f xs
                       | otherwise = Cons x $ deleteIf f xs

deleteEvens' = deleteIf even
deleteMod3' = deleteIf (\x -> x`mod`3==0)

data Expr = Val Int
          | Sum Expr Expr

eval :: Expr -> Int
eval (Val x) = x
eval (Sum l r) = eval l + eval r

