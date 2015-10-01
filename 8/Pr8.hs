
-- Аппликативные функторы, см. строку 52 и далее
import Control.Applicative
-- Монады, см. строку 78 и далее
import Control.Monad

data SomeType = C1 Int
              | C2 Bool
                deriving (Eq, Show)

-- Классы типов
                         
class Show' a where
    show' :: a -> String

class Eq' a where
    (===) :: a -> a -> Bool

-- Экземпляры
             
instance Show' SomeType where
    show' (C1 x) = "C1 " ++ show x
    show' (C2 y) = if y then "C2" else "!C2"

instance Eq' SomeType where
    C1 x === C1 y = x == y
    C2 x === C2 y = x == y
    _ === _ = False

showEquality :: (Eq' a, Show' a) => a -> a -> String
showEquality x y = if x === y
                   then show' x ++ " === " ++ show' y
                   else show' x ++ " =/= " ++ show' y

-- Функтор

class Functor' f where
    fmap' :: (a -> b) -> f a -> f b

-- Закон:
-- fmap id  ==  id
             
instance Functor' [] where
    fmap' g [] = []
    fmap' g (x:xs) = g x : fmap' g xs

instance Functor' Maybe where
    fmap' g Nothing = Nothing
    fmap' g (Just x) = Just $ g x

instance Functor' (Either c) where
    fmap' g (Left z) = Left z
    fmap' g (Right x) = Right $ g x

instance Functor' ((->) c) where
    fmap' = (.)

-- Аппликативные функторы
-- из Control.Applicative
            
class Functor' f => Applicative' f where
    pure' :: a -> f a
    (<*>>) :: f (a -> b) -> f a -> f b

-- Законы:
-- pure id <*> v  ==  v
-- pure (.) <*> u <*> v <*> w  == u <*> (v <*> w)
-- pure f <*> pure x  ==  pure (f x)
-- u <*> pure y  ==  pure ($ y) <*> u

instance Applicative' Maybe where
    pure' x = Just x

    Nothing <*>> _ = Nothing
    _ <*>> Nothing = Nothing
    Just g <*>> Just x = Just $ g x

-- Монады
                         
class Applicative' m => Monad' m where
    (>>==) :: m a -> (a -> m b) -> m b -- >>=

add :: Int -> Int -> Maybe Int
add x y = return $ x+y

mul :: Int -> Int -> Maybe Int
mul x y = return $ x*y

divide :: Int -> Int -> Maybe Int
divide x 0 = Nothing
divide x y = Just $ x`div`y

safeExpr :: Int -> Int -> Int -> Int -> Maybe Int
safeExpr x y z t = do
  x' <- return x
  y' <- return y
  a <- add x' y'
  b <- mul z t
  c <- divide a b
  add c 1

safeExpr' x y z t = pure x >>== \x' -> pure y >>== \y' -> add x' y' >>== \a -> mul z t >>== \b -> divide a b >>== \c -> add c 1
-- НИЧОСИ?!
              
-- Законы:
-- return a >>= k  ==  k a
-- m >>= return  ==  m
-- m >>= (\x -> k x >>= h)  ==  (m >>= k) >>= h
-- fmap f xs  ==  xs >>= return . f          

instance Monad' Maybe where
    Nothing >>== f  = Nothing
    Just x >>== f  = f x

primes :: [Integer]
primes = do
  x <- [2..]
  guard $ all (/=0) $ map (mod x) [2..x-1]
  return x

cartesian :: [a] -> [b] -> [(a,b)]
cartesian as bs = do
  a <- as
  b <- bs
  return (a,b)

instance Applicative' [] where
    pure' x = [x]
         
instance Monad' [] where
    as >>== f = concatMap f as

