import           Control.Arrow ((>>>))
import           Control.Monad (guard)
import           Data.Functor  ((<&>))
import           Data.List     (delete, nub, sort, transpose)

main :: IO ()
main = do
    xs <- getContents <&> lines

    let ans = sort $ nub $ do
            x <- xs
            y <- delete x xs
            z <- delete y (delete x xs)
            guard (all (nub >>> length >>> (`elem`[1,3])) (transpose [x,y,z]))
            pure (sort [x,y,z])

    putStr $ if null ans
        then "Engin Mengi\n"
        else unlines (map unwords ans)
