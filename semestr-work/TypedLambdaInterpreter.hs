module TypedLambda where 

data Type = Logic
          | Nat
          | Composite Type Type
          | Error
          deriving(Eq)

data Term = TVar String
          | TAbs String Type Term
          | TApp Term Term
          | TTrue
          | TFalse
          | TError
          deriving(Show, Eq)

instance Show Type where
    show Logic = "Logic"
    show Nat = "Nat"
    show (Composite t1 t2) = "(" ++ (show t1) ++ " -> " ++ (show t2) ++ ")"
    show Error = "Error"

deriveType :: Term -> Type
deriveType term = extractContext (deriveType' [] term) -- may be do the external extract context fun here
    where deriveType' :: [(String, Type)] -> Term -> [(String, Type)]
          deriveType' context TTrue = context ++ [("true", Logic)]
          deriveType' context TFalse = context ++ [("false", Logic)]
          deriveType' _ TError = [("error", Error)]
          deriveType' ((cVarName, cVarType):context) (TVar varName) | cVarName == varName =
                ((cVarName, cVarType):context) ++ [(varName, cVarType)]
          deriveType' ((cVarName, cVarType):context) (TVar varName) | otherwise =
                [(cVarName, cVarType)] ++ (deriveType' context (TVar varName))
          deriveType' [] (TVar _) = [("error", Error)]
          deriveType' context (TAbs argName argType term) = deriveType' (context ++ [(argName, argType)]) term
          deriveType' context (TApp (TAbs absArg argType absTerm) newTerm) | argType == (getType context newTerm) =
                deriveType' context (replaceVars absTerm absArg newTerm)
          deriveType' context (TApp (TAbs absArg absType absTerm) newTerm) | otherwise                            =
                [("error", Error)]
          deriveType' context (TApp term1 term2) = (deriveType' (deriveType' context term1) term2)

getType :: [(String, Type)] -> Term -> Type
getType ((cVarName, cVarType):context) (TTrue) = Logic
getType ((cVarName, cVarType):context) (TFalse) = Logic
getType ((cVarName, cVarType):context) (TError) = Error
getType ((cVarName, cVarType):context) (TVar varName) | cVarName == varName = cVarType
getType ((cVarName, cVarType):context) (TVar varName) | otherwise           = getType context (TVar varName)
getType ((cVarName, cVarType):context) (TAbs argName argType body) | cVarName == argName =
    Composite argType (getType ((cVarName, cVarType):context) body)
getType ((cVarName, cVarType):context) (TAbs argName argType body) | otherwise           =
    getType ((cVarName, cVarType):context) body
getType ((cVarName, cVarType):context) (TApp term1 term2) =
    Composite (getType ((cVarName, cVarType):context) term1) (getType ((cVarName, cVarType):context) term2)
getType [] (TVar varName) | otherwise = Error

extractContext :: [(String, Type)] -> Type
extractContext [] = Error
extractContext ((_, cType):[]) = cType
extractContext ((_, cType):lastContext) = Composite cType (extractContext lastContext)

replaceVars :: Term -> String -> Term -> Term
replaceVars (TVar varName) argName newTerm | varName == argName = newTerm
replaceVars (TVar varName) _       _       | otherwise          = (TVar varName)
replaceVars (TAbs absArg absType body) argName newTerm = TAbs absArg absType (replaceVars body argName newTerm)
replaceVars (TApp t1 t2) argName newTerm = TApp (replaceVars t1 argName newTerm) (replaceVars t2 argName newTerm)
replaceVars TTrue _ _ = TTrue
replaceVars TFalse _ _ = TFalse
replaceVars _ _ _ = TError
