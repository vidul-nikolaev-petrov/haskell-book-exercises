module HuttonRazor where

data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit int) = int
eval (Add x y) = eval x + eval y

exp1 :: Expr
exp1 = Lit 10

exp2 :: Expr
exp2 = Lit 20

exp3 :: Expr
exp3 = Add exp1 exp2

exp4 :: Expr
exp4 = Add exp3 (Lit 100)

printExpr :: Expr -> String
printExpr (Lit int) = show int
printExpr (Add x y) = printExpr x ++ " + " ++ printExpr y

main :: IO ()
main = do
  print $ eval exp1
  print $ eval exp4
  print $ eval (Add (Lit 1) (Lit 9001))
  print $ printExpr (Add (Lit 1) (Lit 9001))
  print $ printExpr exp4