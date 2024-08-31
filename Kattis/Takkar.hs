import           Control.Arrow ((&&&), (>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    x:y:_ <- getContents <&> (words >>> map read) :: IO [Int]
    putStrLn $ case compare x y of
        LT -> "FAKE NEWS!"
        EQ -> "WORLD WAR 3!"
        GT -> "MAGA!"
