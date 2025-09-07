import           Control.Arrow         (second, (***), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.Map.Lazy         as M
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:rest <- C.getContents <&> C.lines

    let (raw,comp) = map C.words rest
            & splitAt (readInt n)
            & (map (\[n,c] -> (n,readInt c)) >>> M.fromList)
                ***
              (drop 1 >>> map (\(n:_:xs) -> (n,parseParts xs)))

        cost :: M.Map C.ByteString Int
        cost = M.union raw (M.fromList (map (second (map (\(m,a) -> cost M.! m * a) >>> sum)) comp))

    print (cost M.! C.pack "Capstone")

parseParts :: [C.ByteString] -> [(C.ByteString,Int)]
parseParts []       = []
parseParts (x:y:xs) = (x,readInt y) : parseParts xs

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
