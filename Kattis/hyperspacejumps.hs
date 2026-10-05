import           Control.Arrow         ((>>>))
import           Control.Monad         (when)
import           Data.Array.Base       (assocs, newArray, unsafeRead,
                                        unsafeWrite, (!))
import           Data.Array.ST         (runSTArray, runSTUArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.List             (find)

main :: IO ()
main = do
    [d]:_:xs <- getContents <&> (lines >>> map (words >>> map read))

    let graph = runSTArray $ do
            ret <- newArray (0,500) []
            mapM_ (\[a,b] -> do
                unsafeRead ret a >>= ((b:) >>> unsafeWrite ret a)
                unsafeRead ret b >>= ((a:) >>> unsafeWrite ret b)
                ) xs
            pure ret

        t = maximum (concat xs)

        f :: [(Int,Int)] -> [(Int,Int)]
        f prev = assocs $ runSTUArray $ do
            ret <- newArray (0,500) 0
            mapM_ (\(u,x) -> when (x>0) (mapM_ (\v -> unsafeRead ret v >>= ((x+) >>> unsafeWrite ret v)) (graph ! u))) prev
            pure ret

    iterate f [(0,1)]
        & take (d+1)
        & map (find (fst >>> (==t)) >>> maybe 0 snd)
        & sum
        & print

