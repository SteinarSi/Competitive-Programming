import           Control.Arrow ((>>>))
import           Data.List     (find)

main :: IO ()
main = do
    x <- read <$> getLine
    let Just (c,_) = find (snd >>> (>=x)) [(500,500),(1000,1000),(2000,2000),(5000,5250),(10000,11000),(20000,24000)]
    print c
