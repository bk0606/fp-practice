
{- ghci -- запускает REPL -}

{- ghci File.hs -}
{- ghc File.hs -}

hello a b = a + b

{- hello 2 3 -- вызов
   hello 2 (hello 3 4)
   hello 2 $ hello 3 4 -}

factorial n = if n==0
              then 1
              else n*factorial (n-1)

pointlessFunction n = case n of
                        1 -> 2
                        3 -> 5
                        x -> 2*x
{- (+) 1 2
   1 `hello` 3 -}

-- comment

gcd' a b = if a < b
           then gcd' b a
           else if a`mod`b == 0
                then b
                else gcd' b $ a`mod`b

-- /=  -- не равно

myGCD :: Int -> Int -> Int
myGCD a 0 = a
myGCD a b | a<b = myGCD b a
          | otherwise = myGCD b $ a`mod`b
{-
Bool
Int
Integer
Double 
-}

fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib n | n > 2 = fib (n-1) + fib (n-2)
      | otherwise = 0

fac n = fac' n 1
    where fac' 1 acc = acc
          fac' n acc | n>1 = fac' (n-1) $ n*acc

fibo :: Int -> Integer
fibo n | n>0 = let
    fibo' 1 a b = a
    fibo' 2 a b = b
    fibo' n a b = fibo' (n-1) b $ a+b
 in fibo' n 1 1

fun :: Int -> Double -> Double
fun a b = fromIntegral a / b
    
main = print $ hello 1 2

