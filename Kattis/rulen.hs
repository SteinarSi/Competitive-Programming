import           Control.Arrow      ((>>>))
import           Data.Array.Unboxed (UArray, bounds, elems, inRange, indices,
                                     listArray, (!))
import           Data.Bool          (bool)
import           Data.Function      ((&))
import           Data.Functor       ((<&>))
import           Text.Printf        (printf)

main :: IO ()
main = do
    [n,l,r] <- getLine <&> (words >>> map read)

    xs <- getLine <&> map ('1'==)
    let start = listArray (-99,l+100) (replicate 100 False <> xs <> replicate 100 False)
        rule = printf "%b" n
            & map ('1'==)
            & reverse
            & listArray ((False,False,False),(True,True,True))

    iterate (generate rule) start
        & drop 1
        & take r
        & map (format l)
        & unlines
        & putStr

format :: Int -> UArray Int Bool -> String
format l gen = map ((gen!) >>> bool '0' '1') [1..l]

generate :: UArray (Bool,Bool,Bool) Bool -> UArray Int Bool -> UArray Int Bool
generate rule gen = listArray (bounds gen) (map next (indices gen))
  where
    next i = rule ! (
        gen !!! (i-1),
        gen !!! i,
        gen !!! (i+1)
        )

(!!!) :: UArray Int Bool -> Int -> Bool
(!!!) arr i | inRange (bounds arr) i = arr ! i
            | otherwise              = False
