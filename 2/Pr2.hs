
{- синонимы типов -}
type MyFun = Integer -> Integer
type Fun2 = Integer -> Integer

doubleApply :: MyFun -> MyFun
-- doubleApply :: (Integer -> Integer) -> (Integer -> Integer)
doubleApply f x = f (f x)

otherDouble :: Fun2 -> Fun2
otherDouble = doubleApply

{- newtype -}
newtype TypeName = TypeConst Int
f1 :: TypeName
f1 = TypeConst 5

f2 (TypeConst x) = x

newtype Dollars = Dollars Int
newtype Rubles = Rubles Int

-- -- compilation fails!
-- myBankIsStupid :: Rubles -> Dollars
-- myBankIsStupid r = r

{- data -}
data Type = Const1
          | Const2 Int
          | Const3 Double Int
          | Const4 Int

g1 :: Int -> Type
g1 0 = Const1
g1 x | x`mod`3 == 0 = Const2 x
     | x`mod`3 == 1 = Const3 (fromIntegral x/3) x
     | x`mod`3 == 2 = Const4 $ x`mod`2

g2 :: Type -> Int
g2 Const1 = 1
g2 (Const2 x) = 2*x
g2 (Const3 x y) = truncate $ x*fromIntegral y
g2 (Const4 x) = 3*x

g3 :: Type -> Double
g3 (Const3 x y) = x
-- g3 z = error "Wrong constructor"

data Type2 = Con1
           | Con2
             { amount :: Int }
           | Con3
             { count :: Int
             , len :: Double }

g4 = Con2 5
g5 = Con3 { len=4.5, count=5 }
g6 = Con3 5 4.5

{-
data Type2 = Con1
           | Con2 Int
           | Con3 Int Double
amount :: Type2 -> Int
amount (Con2 x) = x
amount z = error "..."

count :: Type2 -> Int
count (Con3 x y) = x
count z = error "..."

length :: Type2 -> Double
length (Con3 x y) = y
length z = error "..."
-}

newtype TypeA = TypeA Int
data TypeB = TypeB Int
data TypeC = TypeC Int

-- -- compilation fails!
-- fail1 :: TypeB -> TypeC
-- fail1 x = x

data Notebook = Macbook | Notebook { depth :: Double }

hipsterReaction :: Notebook -> String
hipsterReaction Macbook = "happy!!!111"
hipsterReaction (Notebook x) | x < 1.0 = "happy!"
                             | otherwise = "okay :/"

newtype Workspace = Workspace { unWorkspace :: Notebook }

notebookDepth x = case x of
                    Macbook -> 0.9
                    y -> depth x

depth2 x = depth x / 2

-------- СПИСКИ

myLen :: [Int] -> Int
myLen [] = 0
myLen (x:xs) = 1 + myLen xs

mySum :: [Int] -> Int
mySum [] = 0
mySum (x:xs) = x + mySum xs

{- mySum [1,2,3] -}
{- [1,2,3] == 1:2:3:[] -}

sumBy2 :: [Int] -> [Int]
sumBy2 [] = []
sumBy2 [x] = [x]
sumBy2 (x:y:xs) = x+y : sumBy2 xs

