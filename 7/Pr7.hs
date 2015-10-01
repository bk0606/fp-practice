
type Set a = a -> Bool

emptySet :: Set a
emptySet = \x -> False

singleton :: (Eq a) => a -> Set a
singleton y = \x -> x==y

contains :: a -> Set a -> Bool
contains y s = s y

add :: (Eq a) => a -> Set a -> Set a
add y s = \x -> x==y || s x

intersection :: Set a -> Set a -> Set a
intersection s1 s2 = \x -> s1 x && s2 x

union :: Set a -> Set a -> Set a
union s1 s2 = \x -> s1 x || s2 x

fromList :: (Eq a) => [a] -> Set a
fromList = foldr add emptySet

----------------------------------------
-- MORSE CODE

decode = map decodeLetter . words

data Tree a = Leaf
            | Branch { tag :: a
                     , left :: Tree a
                     , right :: Tree a
                     }
              deriving (Show)

-- instance Show a => Show (Tree a) where
--     show Leaf = "_"
--     show (Branch c x y) = "(" ++ show c ++ show x ++ ", " ++ show y ++ ")"

program :: String
program = "__5__4H___3VS__F___2 UI__L__+_ R__P___1JWAE"
       ++ "__6__=B__/_XD__C__YKN__7_Z__QG__8_ __9__0 OMT "
                       
-- dict = interpret program
--     where interpret = head . foldl exec []
--           exec st '_' = Leaf : st
--           exec (x:y:st) c = Branch c y x : st

-- decodeLetter :: String -> Char
-- decodeLetter = tag . foldl step dict
--     where step d '.' = left d
--           step d '-' = right d

branch :: Char -> (String -> Char) -> (String -> Char) -> (String -> Char)
branch c f g [] = c
branch c f g ('.':xs) = f xs
branch c f g ('-':xs) = g xs

leaf = \x -> ' '
                        
-- dict' = combinator ' ' (combinator 'E' ... ...) (combinator 'T' ... ...)

dict = interpret program
    where interpret = head . foldl exec []
          exec st '_' = leaf : st
          exec (x:y:st) c = branch c y x : st

decodeLetter :: String -> Char
decodeLetter = dict
                            
