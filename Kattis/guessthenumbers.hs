import           Control.Arrow ((>>>))
import           Data.Bool     (bool)
import           Data.Char     (isAlpha)
import           Data.Function ((&))
import           Data.List     (permutations)
import           Data.Maybe    (fromJust)

data Expr =
      Var Char
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
  deriving (Show)

main :: IO ()
main = getContents >>= (
            lines
        >>> parse
        >>> map (solve >>> bool "NO" "YES")
        >>> unlines
        >>> putStr
    )

solve :: (Expr,[Char],[Int],Int) -> Bool
solve (expr,vars,vals,y) = any (\perm -> eval (zip vars perm) expr == y) (permutations vals)

parse :: [String] -> [(Expr,[Char],[Int],Int)]
parse [_] = []
parse (xs:ys:xss) = (ex,vars,vals,y) : parse xss
  where
    vars = filter isAlpha ys
    (ex,"") = parseExpr ys
    y:vals = words xs
        & drop 1
        & reverse
        & map read

parseExpr :: String -> (Expr, String)
parseExpr "" = error "bruh"
parseExpr ('(':xs) = let (a, ')':xs') = parseExpr xs
                     in  case xs' of
    "" -> (a, "")
    ')':_ -> (a, xs')
    '+':xs' -> let (b, xs'') = parseExpr xs'
               in  (Add a b, xs'')
    '-':xs' -> let (b, xs'') = parseExpr xs'
               in  (Sub a b, xs'')
    '*':xs' -> let (b, xs'') = parseExpr xs'
               in  (Mul a b, xs'')
parseExpr (a:xs) = case xs of
    "" -> (Var a, "")
    ')':_ -> (Var a, xs)
    '+':xs' -> let (b, xs'') = parseExpr xs'
               in  (Add (Var a) b, xs'')
    '-':xs' -> let (b, xs'') = parseExpr xs'
               in  (Sub (Var a) b, xs'')
    '*':xs' -> let (b, xs'') = parseExpr xs'
               in  (Mul (Var a) b, xs'')
    _ -> error ("bruh: " <> (a:xs))

eval :: [(Char,Int)] -> Expr -> Int
eval vals (Var x)   = fromJust (lookup x vals)
eval vals (Add a b) = eval vals a + eval vals b
eval vals (Sub a b) = eval vals a - eval vals b
eval vals (Mul a b) = eval vals a * eval vals b
