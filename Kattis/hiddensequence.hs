import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))

main :: IO ()
main = do
    [xs,ys,zs] <- C.getContents <&> (C.lines >>> map C.unpack)
    putStrLn (solve xs ys zs)

solve :: [Char] -> [Char] -> [Char] -> String
solve [] [] []             = ""
solve xs ('1':ys) ('1':zs) = '1' : solve xs ys zs
solve ('2':xs) ys ('2':zs) = '2' : solve xs ys zs
solve ('3':xs) ('3':ys) zs = '3' : solve xs ys zs
