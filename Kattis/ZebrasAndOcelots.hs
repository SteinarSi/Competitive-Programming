import           Control.Arrow ((>>>))
import           Data.Bits     (shiftL)
import           Data.Bool     (bool)
import           Data.Function ((&))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    n:xs <- getContents <&> words

    let goal  = 1 `shiftL` read n - 1 :: Int
        start = map ((=="Z") >>> bool 0 1) xs
            & reverse
            & zipWith (*) (map (shiftL 1) [0..])
            & sum

    print (goal - start)
