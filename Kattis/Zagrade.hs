import           Control.Arrow ((>>>))
import           Data.Function ((&))
import qualified Data.IntSet   as IS
import qualified Data.Set      as S

main :: IO ()
main = do
    expr <- getLine
    parse [] (zip [0..] expr)
        & reverse
        & combinations IS.empty
        & map (`format` expr)
        & S.fromList
        & S.toAscList
        & unlines
        & putStr

format :: IS.IntSet -> String -> String
format set xs = [x | (i,x) <- zip [0..] xs, IS.notMember i set]

combinations :: IS.IntSet -> [(Int,Int)] -> [IS.IntSet]
combinations set [] | IS.null set = []
                    | otherwise  = [set]
combinations set ((i,j):xs) = combinations set xs <> combinations (IS.insert i (IS.insert j set)) xs

parse :: [Int] -> [(Int,Char)] -> [(Int,Int)]
parse _ []                = []
parse is ((i,'('):xs)     = parse (i:is) xs
parse (i:is) ((j,')'):xs) = (i,j) : parse is xs
parse is (_:xs)           = parse is xs
