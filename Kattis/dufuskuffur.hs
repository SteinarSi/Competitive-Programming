import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [n,m] <- getContents <&> (words >>> map read)
    putStrLn $ case compare (n::Int) m of
        LT -> "Dufur passa"
        EQ -> "Dufur passa fullkomlega"
        GT -> "Dufur passa ekki"
