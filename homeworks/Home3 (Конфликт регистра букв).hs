
data Expr = Val Int
          | Plus Expr Expr
          | Minus Expr Expr
          | Mult Expr Expr
          | Div Expr Expr
          | Mod Expr Expr

eval :: Expr -> Int
eval (Val value)         = value
eval (Plus expr1 expr2)  = eval (expr1) + eval (expr2)
eval (Minus expr1 expr2) = eval (expr1) - eval (expr2)
eval (Mult expr1 expr2)  = eval (expr1) * eval (expr2)
eval (Div expr1 expr2)   = eval (expr1) `div` eval (expr2)
eval (Mod expr1 expr2)   = eval (expr1) `mod` eval (expr2)
