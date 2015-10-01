
import Control.Applicative
import Data.Either

{-

Распознать язык лямбда-исчисления

((\x . x) (\x . (\y . y)))

-}

data Term = Var String
          | Lam String Term
          | App Term Term
            deriving (Show)
    
type ErrorMsg = String
newtype Parser a = Parser { runParser :: String -> Either ErrorMsg [(a, String)] }

instance Functor Parser where
    fmap f (Parser g) = Parser $ \s -> case g s of
                                         Left err -> Left err
                                         Right values -> Right $ map (\(a,rest) -> (f a, rest)) values

instance Applicative Parser where
    pure a = Parser $ \s -> Right [(a, s)]
    Parser fun <*> Parser val = Parser $ \s -> case fun s of
                                                 Left err -> Left err
                                                 Right funs -> Right $ concatMap (apply val) funs
                                                     where apply val (f,rest) = case val rest of
                                                                                  Left err' -> []
                                                                                  Right vals -> map (\(v,rest') -> (f v, rest')) vals

anyChar :: Parser Char
anyChar = Parser $ \s -> case s of
                           [] -> Left "No chars left"
                           (c:cs) -> Right [(c,cs)]

char :: Char -> Parser Char
char ch = Parser $ \s -> case s of
                           [] -> Left "No chars left"
                           (c:cs) | c /= ch -> Left $ [ch] ++ " was expected, but " ++ [c] ++ " was found"
                                  | otherwise -> Right [(c,cs)]

string :: String -> Parser String
string [] = Parser $ \s -> Right [([],s)]
string (c:cs) = (:) <$> char c <*> string cs
-- (<$>) f a = pure f <*> a

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \s -> case s of
                             [] -> Left "No chars left"
                             (c:cs) | f c -> Right [(c,cs)]
                                    | otherwise -> Left $ "Unexpected char " ++ [c]

letter :: Parser Char
letter = satisfy (`elem`['A'..'Z'] ++ ['a'..'z'])

digit :: Parser Char
digit = satisfy (`elem`['0'..'9'])

spaces :: Parser String
spaces = many $ char ' '
        
instance Alternative Parser where
    empty = Parser $ \s -> Right []
    Parser a <|> Parser b = Parser $ \s -> case a s of
                                             Left err -> case b s of
                                                           Left err' -> Left err'
                                                           Right bs -> Right bs
                                             Right as -> Right as
-- many elem = ((:) <$> elem <*> many elem) <|> empty

bracketed parser = char '(' *> parser <* char ')'

var :: Parser String
var = (:) <$> letter <*> many (letter <|> digit)

lam :: Parser Term
lam = Lam <$> (char '\\' *> var <* spaces <* char '.' <* spaces) <*> term

app :: Parser Term
app = App <$> term <* spaces <*> term

term = (Var <$> var)
   <|> bracketed (lam
              <|> app)

instance Monad Parser where
    return a = pure a
    Parser ma >>= f = Parser $ \s -> case ma s of
                                       Left err -> Left err
                                       Right as -> Right $ concat $ rights $ map (\(a,rest) -> runParser (f a) rest) as

lam' = do
  char '\\'
  x <- var
  spaces
  char '.'
  spaces
  t <- term
  return $ Lam x t

                                                   
