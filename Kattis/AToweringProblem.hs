import           Control.Arrow ((>>>))
import           Control.Monad (guard)
import           Data.Function (on, (&))
import           Data.Functor  ((<&>))
import           Data.List     (delete, sortBy)

main :: IO ()
main = do
    xs <- getLine <&> (words >>> map read)
    let (s, [h1, h2]) = splitAt 6 xs
    let tower1 = head $ do
            x <- s
            y <- delete x s
            guard (x >= y)
            z <- delete y (delete x s)
            guard (y >= z)
            guard (x + y + z == h1)
            pure [x,y,z]
        tower2 = sortBy (compare `on` negate) (foldr delete s tower1)
    putStrLn . unwords $ map show (tower1 ++ tower2)
