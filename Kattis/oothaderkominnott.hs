import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    xs <- getContents <&> (words >>> drop 1 >>> map read)
    print $ case xs of
        [w]     -> size w w 3
        [w,l]   -> size w l 3
        [w,l,h] -> size w l h

size :: Int -> Int -> Int -> Int
size w l h = 2*w*h + 2*(l-2)*h + (w-2)*(l-2)
