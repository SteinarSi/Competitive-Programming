import           Control.Arrow         (first, (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (isDigit)
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (isPrefixOf)
import           Text.Printf           (printf)

main :: IO ()
main = do
    nm:rest <- C.getContents <&> C.lines
    let Just (n,_) = C.readInt nm
        (patterns,queries)   = splitAt n (map (C.unpack >>> parse) rest)
        match xs = zip patterns [1..]
            & filter (fst >>> (`isPrefixOf` xs))
            & map (first length)
            & ((0,-1):)
            & maximum
            & snd
            & show
    map match queries
        & unlines
        & putStr

parse :: String -> String
parse xs
    | null e = nums
    | otherwise   = take (read (drop 1 e)) nums
  where
    (a,_:xs')   = span isDigit xs
    (b,_:xs'')  = span isDigit xs'
    (c,_:xs''') = span isDigit xs''
    (d,e)  = span isDigit xs'''
    nums = concatMap (read @Int >>> printf "%.8b") [a,b,c,d]
