
-- data Either a b = Left a
--                 | Right b

eTest1 :: Bool -> Either String Int
eTest1 False = Left "asdasdsad"
eTest1 True = Right 123

lefts :: [Either a b] -> [a]
lefts [] = []
lefts ((Left x):xs) = x : lefts xs
lefts ((Right x):xs) = lefts xs

rights :: [Either a b] -> [b]
rights [] = []
rights (x:xs) = case x of
                  Left a -> rights xs
                  Right b -> b : rights xs

rights' xs = map fromRight (filter isRight xs)
    where isRight (Left _) = False
          isRight (Right _) = True
          fromRight (Right x) = x

myEither :: (a -> c) -> (b -> c) -> Either a b -> c
myEither f g (Left a) = f a
myEither f g (Right b) = g b

partitionEithers :: [Either a b] -> ([a],[b])
partitionEithers [] = ([],[])
partitionEithers (x:xs) = case x of
                            Left a -> (a:as, bs)
                            Right b -> (as, b:bs)
    where (as,bs) = partitionEithers xs

pE :: [Either a b] -> ([a],[b])
pE xs = go xs ([],[])
    where go [] (as,bs) = (reverse as, reverse bs)
          go ((Left a):xs) (as,bs) = go xs (a:as, bs)
          go ((Right b):xs) (as,bs) = go xs (as, b:bs)

pE' :: [Either a b] -> ([a],[b])
pE' = foldr f ([],[])
    where f (Left a)  (as,bs) = (a:as, bs)
          f (Right b) (as,bs) = (as, b:bs)
                                      
