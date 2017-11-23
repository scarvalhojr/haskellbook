
data Expr = Lit Integer | Add Expr Expr deriving Show

eval :: Expr -> Integer
eval (Lit i)   = i
eval (Add x y) = (eval x) + (eval y)

printExpr :: Expr -> String
printExpr (Lit i)
  | i < 0     = "(" ++ show i ++ ")"
  | otherwise = show i
printExpr (Add x y) = "(" ++ (printExpr x) ++ " + " ++ (printExpr y) ++ ")"

a1 = Add (Lit 9001) (Lit 1)
a2 = Add a1 (Lit 20001)
a3 = Add (Lit 1) a2
