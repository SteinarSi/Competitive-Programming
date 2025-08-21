import           Control.Arrow ((>>>))
import           Data.Function ((&))
import           Data.Functor  ((<&>))
import           Data.List     (find)

main :: IO ()
main = do
    [c,x,d]:xs <- getContents <&> (lines >>> map (words >>> map read))
    reverse xs
        & find (\[v,e] -> d / e + x * d / v < c / 2)
        & maybe "NO" (head >>> round >>> show >>> ("YES " <>))
        & putStrLn
