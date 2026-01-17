import           Control.Arrow ((>>>))
import           Data.Char     (isAlpha, isDigit)
import           Text.Printf   (printf)

data Expr =
      Lit Int
    | Var String
    | Op String Expr Expr

main :: IO ()
main = interact (
            lines
        >>> map (words >>> parse >>> fst >>> simplify >>> show)
        >>> zipWith (printf "Case %i: %s") [1::Int ..]
        >>> unlines
    )

parse :: [String] -> (Expr,[String])
parse (x:xs)
    | any isDigit x = (Lit (read x), xs)
    | any isAlpha x = (Var x, xs)
    | otherwise     = let (l,xs' ) = parse xs
                          (r,xs'') = parse xs'
                      in  (Op x l r, xs'')

simplify :: Expr -> Expr
simplify (Lit x) = Lit x
simplify (Var v) = Var v
simplify (Op o a b) = case (simplify a, simplify b) of
    (Lit x, Lit y) -> Lit (op x y)
    (x, y)         -> Op o x y
  where
    op = case o of
        "+" -> (+)
        "-" -> (-)
        "*" -> (*)

instance Show Expr where
    show (Var v)    = v
    show (Lit x)    = show x
    show (Op o a b) = unwords [o, show a, show b]
