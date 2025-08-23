import           Control.Arrow         ((>>>))
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.List             (isPrefixOf)

main :: IO ()
main = C.getLine >>= (
            C.foldr (('1'==) >>> (:)) []
        >>> compress 1
        >>> map (bool '0' '1')
        >>> putStrLn
    )

compress :: Int -> [Bool] -> [Bool]
compress i xs | not (atLeast (2*i) xs) = xs
              | otherwise = compress (i+1) (comp xs)
  where
    comp :: [Bool] -> [Bool]
    comp ys | not (atLeast (2*i) ys) = ys
            | zs `isPrefixOf` ws     = comp ws
            | otherwise              = head ys : comp (tail ys)
      where
        (zs,ws) = splitAt i ys

atLeast :: Int -> [a] -> Bool
atLeast 0 _      = True
atLeast _ []     = False
atLeast i (_:xs) = atLeast (i-1) xs
