module Main where 


data Term = Variable String
          | Abstract String Term
          | Application Term Term


eval :: Term -> Term
eval (Variable var) = Variable var
eval (Abstract var body) = Abstract var (eval body) -- can be not evaluated
eval (Application (Variable _) term2) = eval term2
eval (Application (Abstract var body) term2) = eval (replaceVars body var term2)
eval (Application term1 term2) = eval $ Application (eval term1) (eval term2)

replaceVars :: Term -> String -> Term -> Term
replaceVars (Abstract currVar term) var newTerm | currVar == var =
        Abstract (var ++ (show 0)) (replaceVars (findAndRename term var) var newTerm)
replaceVars (Abstract currVar term) var newTerm | otherwise      =
        Abstract (currVar) (replaceVars (findAndRename term var) var newTerm)
replaceVars (Application t1 t2) var newTerm = Application (replaceVars t1 var newTerm) (replaceVars t2 var newTerm)
replaceVars (Variable currVar) var newTerm | currVar == var = newTerm
replaceVars (Variable currVar) _   _       | otherwise      = Variable currVar
replaceVars term var newTerm = term


findAndRename :: Term -> String -> Term
findAndRename term name = fAndR' term name 0
    where   fAndR' :: Term -> String -> Int -> Term
            fAndR' (Variable oldName) name ind | oldName == name = Variable (name ++ (show ind))
            fAndR' (Variable oldName) _    _   | otherwise       = Variable (oldName)
            fAndR' (Application t1 t2) name ind = Application (fAndR' t1 name ind) (fAndR' t1 name ind)
            fAndR' (Abstract oldName t) name ind | oldName == name = Abstract (name ++ (show (ind+1))) (fAndR' t name (ind+1))
            fAndR' (Abstract oldName t) name ind | otherwise       = Abstract (oldName) (fAndR' t name (ind))


toString :: Term -> String
toString (Variable var) = var
toString (Abstract term1 term2) = "L " ++ (term1) ++ "." ++ (toString term2)
toString (Application term1 term2) = "(" ++ (toString term1) ++ ") (" ++ (toString term2) ++ ")"


main :: IO()
main = print $ toString $ eval $
    Application (
        Application (
            Abstract "a" (Abstract "b" (Abstract "t" (Abstract "f" (Variable "a"))))
        ) (
            Application (Application (Variable "b") (Variable "t")) (Variable "f")
        )
    ) (
        Abstract "t" (Variable "t")
    )
--(Application
--    (Application
--        (Application
--            ((Abstract "a" (Abstract "b" (Abstract "t" (Abstract "f" (Variable "a"))))))
--            (Application (Application (Variable "b") (Variable  "t")) (Variable "f")))
--        (Application (Variable "t") (Variable "f")))
--    (Abstract "t" (Abstract "f" (Variable "t")))) 0

-- toString (eval(Abstract "a" (Application (Abstract "b" (Variable "b")) (Variable "a"))))
-- Application ((Abstract "x" (Abstract "y" (Variable "x")))) (Variable "y")
-- Application (Abstract "x" (Abstract "y" (Variable "y"))) (Variable "z")
-- eval(Application (Application (Variable "x") (Variable "y")) (Variable "z"))
