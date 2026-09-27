import           Control.Arrow      ((>>>))
import           Control.Monad      (unless)
import           Data.Array.Unboxed (Array, UArray, elems, listArray, (!))
import           Data.List          (find)
import           System.IO          (hFlush, stdout)

main :: IO ()
main = getLine >>= (read >>> play)

play :: Int -> IO ()
play n = do
    let Just i = find (\j -> n > j && not (win ! (n-j))) [1..3]
    print i
    hFlush stdout
    response <- getLine
    unless (response == "I give up") (play (n - i - read response))

win :: UArray Int Bool
win = listArray (1,1000) (elems lazy)
  where
    lazy :: Array Int Bool
    lazy = listArray (1,1000) (map f [1..1000])

    f 1 = False
    f i = any ((lazy !) >>> not) [i-1,i-2..max 1 (i-3)]
