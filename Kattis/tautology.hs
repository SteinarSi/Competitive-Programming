import           Control.Arrow         (first, second, (&&&), (***), (>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import qualified Data.Map.Strict       as M

data WFF = Var Char
         | And WFF WFF
         | Or WFF WFF
         | Not WFF
         | Implies WFF WFF
         | Equals WFF WFF
    deriving (Show, Eq)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> init
        >>> map (C.unpack
            >>> parse
            >>> fst
            >>> tautology)
        >>> C.unlines
        >>> C.putStr
    )

parse :: String -> (WFF, String)
parse "" = error "bruh"
parse (x:xs) = case x of
        'K' -> binop And
        'A' -> binop Or
        'N' -> first Not (parse xs)
        'C' -> binop Implies
        'E' -> binop Equals
        _ | x `elem` vars -> (Var x, xs)
          | otherwise -> error ("bruh: '" <> [x] <> "'")
    where
        binop :: (WFF -> WFF -> WFF) -> (WFF, String)
        binop op = parse xs
            & op *** parse
            & uncurry first

tautology :: WFF -> C.ByteString
tautology wff = vars
        & assignments
        & all (evaluate wff)
        & bool "not" "tautology"
        & C.pack

evaluate :: WFF -> M.Map Char Bool -> Bool
evaluate wff ass = eval wff
    where
        eval :: WFF -> Bool
        eval (Var x)       = ass M.! x
        eval (And x y)     = eval x && eval y
        eval (Or x y)      = eval x || eval y
        eval (Not x)       = not (eval x)
        eval (Implies x y) = not (eval x) || eval y
        eval (Equals x y)  = eval x == eval y

assignments :: [Char] -> [M.Map Char Bool]
assignments [] = [M.empty]
assignments (x:xs) = assignments xs
        & map (M.insert x True) &&& fmap (M.insert x False)
        & uncurry (<>)

vars :: String
vars = "pqrst"
