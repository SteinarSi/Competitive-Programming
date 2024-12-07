{-# LANGUAGE Strict #-}
import Data.Bool (bool)
import Data.Map (Map, insert, lookup, empty)
import Data.Maybe (fromJust)
import Data.Tuple.Extra ((&&&))
import Prelude hiding (lookup)

main :: IO ()
main = do
    (root1, root2) <- fmap ((toExpr False &&& toExpr True) .  parse . map words . lines) (readFile "inputs/day21-input.txt")
    let Lit result1           = simplify root1
        Eq Var (Lit result2)  = simplify root2
    print (result1, result2)

data Expr = Lit Integer | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr | Eq Expr Expr | Var deriving (Eq, Ord)

instance Num Expr where
    (+) = Add
    (-) = Sub
    (*) = Mul
    fromInteger = Lit

eval :: Expr -> Integer -> Integer
eval Var x = x
eval (Lit a) x = a
eval (Add ex1 ex2) x = eval ex1 x + eval ex2 x
eval (Mul ex1 ex2) x = eval ex1 x * eval ex2 x
eval (Sub ex1 ex2) x = eval ex1 x - eval ex2 x
eval (Div ex1 ex2) x = eval ex1 x `div` eval ex2 x
eval (Eq  ex1 ex2) x = eval ex1 x + eval ex2 x

isConstant :: Expr -> Bool
isConstant (Lit _) = True
isConstant Var = False
isConstant (Add ex1 ex2) = isConstant ex1 && isConstant ex2
isConstant (Sub ex1 ex2) = isConstant ex1 && isConstant ex2
isConstant (Mul ex1 ex2) = isConstant ex1 && isConstant ex2
isConstant (Div ex1 ex2) = isConstant ex1 && isConstant ex2
isConstant (Eq  ex1 ex2) = isConstant ex1 && isConstant ex2

toExpr :: Bool -> Map String [String] -> Expr
toExpr b m = fromJust $ lookup "root" $ toExpr' b "root" empty m
    where
        toExpr' :: Bool -> String -> Map String Expr -> Map String [String] -> Map String Expr
        toExpr' True "humn" ret m = insert "humn" Var ret
        toExpr' b name ret m = case lookup name m of
            Just [lit] -> insert name (Lit (read lit)) ret
            Just [ex1, op, ex2] -> let ret' = toExpr' b ex1 (toExpr' b ex2 ret m) m
                                   in  case (lookup ex1 ret', lookup ex2 ret') of
                                        (Just a, Just b) -> insert name (opr op a b) ret'
                                        _ -> undefined
                where opr _ | name == "root" = Eq
                      opr "+" = Add
                      opr "-" = Sub
                      opr "*" = Mul
                      opr "/" = Div

parse :: [[String]] -> Map String [String]
parse = foldr (\(x:xs) m -> insert (init x) xs m) empty

simplify :: Expr -> Expr 
simplify ex | simp ex == ex = ex
            | otherwise = simplify (simp ex)

simp :: Expr -> Expr 
simp ex | isConstant ex = Lit (eval ex 0)
simp Var = Var
simp (Lit a) = Lit a
simp (Add ex1 ex2) = Add (simp ex1) (simp ex2)
simp (Sub ex1 ex2) = Sub (simp ex1) (simp ex2)
simp (Mul ex1 ex2) = Mul (simp ex1) (simp ex2)
simp (Div ex1 ex2) = Div (simp ex1) (simp ex2)
simp (Eq ex1 ex2)  | isConstant ex1 = simp (Eq ex2 ex1)
simp (Eq (Add a b) ex2) | isConstant a = Eq (simp b) (simp (ex2 - a))
                        | otherwise = Eq (simp a) (simp (ex2 - b))
simp (Eq (Mul a b) ex2) | isConstant a = Eq (simp b) (simp (ex2 `Div` a))
                        | otherwise = Eq (simp a) (simp (ex2 `Div` b))
simp (Eq (Div a b) ex2) | isConstant a = Eq (simp (a `Div` ex2)) (simp b)
                        | otherwise = Eq (simp a) (simp (ex2 * b))
simp (Eq (Sub a b) ex2) | isConstant a = Eq (simp (b + ex2)) (simp a) 
                        | otherwise = Eq (simp a) (simp (b+ex2))
simp (Eq a b) = Eq (simp a) (simp b)
