import           Control.Arrow         (first, (&&&), (***), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromJust)
import           Text.Printf           (printf)

main :: IO ()
main = do
    n:rest <- C.getContents <&> C.lines

    let ((planets,species),spacecraft) = splitAt (readInt n) rest
            & (map C.words
                >>> ((map (head >>> (,0)) >>> M.fromList)
                        &&&
                    (map (\(p:_:ss) -> map (,p) ss) >>> concat >>> M.fromList)
                )
              )
                ***
              (drop 1 >>> map (C.words >>> last &&& (head >>> readInt)))

    spacecraft
        & foldr (first (species M.!) >>> uncurry (M.insertWith (+))) planets
        & M.toAscList
        & map (first C.unpack >>> uncurry (printf "%s %d"))
        & unlines
        & putStr

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
