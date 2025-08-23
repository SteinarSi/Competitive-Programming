import           Control.Arrow         ((>>>))
import           Data.Array.ST         (newArray, runSTUArray, writeArray)
import           Data.Array.Unboxed    (UArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:m:rest <- C.getContents <&> C.words

    let (dict,queries) = splitAt (readInt n) rest

    queries
        & map (match >>> flip filter dict >>> format)
        & C.unlines
        & C.putStr

format :: [C.ByteString] -> C.ByteString
format xs = C.unwords (C.pack (show (length xs)) : xs)

match :: C.ByteString -> C.ByteString -> Bool
match num word = C.length num == C.length word && and (C.zipWith (curry (possible !)) num word)

possible :: UArray (Char,Char) Bool
possible = runSTUArray $ do
    ret <- newArray (('0','a'),('9','z')) False
    mapM_ (\(d,xs) -> mapM_ ((d,) >>> flip (writeArray ret) True) xs) [
        ('0',""),
        ('1',""),
        ('2',"abc"),
        ('3',"def"),
        ('4',"ghi"),
        ('5',"jkl"),
        ('6',"mno"),
        ('7',"pqrs"),
        ('8',"tuv"),
        ('9',"wxyz")
        ]
    pure ret

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
