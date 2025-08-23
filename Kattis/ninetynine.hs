import           Control.Arrow ((>>>))
import           Data.Array    (Array, listArray, (!))
import           Data.Bool     (bool)
import           Data.Functor  ((<&>))
import           System.IO     (hFlush, stdout)

end :: Int
end = 99

main :: IO ()
main = play 0

play :: Int -> IO ()
play x | x   >= end = pure ()
       | x+2 >= end = print end >> hFlush stdout
       | otherwise  = write y >>= play
  where
    y | not (win ! (x+1)) = x+1
      | not (win ! (x+2)) = x+2
      | otherwise         = bool (x+1) (x+2) (even x)

write :: Int -> IO Int
write x = do
    hFlush stdout
    print x
    hFlush stdout
    getLine <&> read

win :: Array Int Bool
win = listArray (0,end) (map f [0..end])
  where
    f x | x   >= end = False
        | x+2 >= end = True
        | otherwise  = not (win ! (x+1)) || not (win ! (x+2))
