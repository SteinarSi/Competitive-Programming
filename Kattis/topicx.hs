import           Control.Arrow           ((>>>))
import           Control.Monad.ST.Strict (ST, runST)
import qualified Data.ByteString.Char8   as C
import           Data.Functor            ((<&>))
import qualified Data.Map.Strict         as M
import           Data.Maybe              (fromJust)
import qualified Data.Set                as S
import           Data.STRef.Strict       (STRef, modifySTRef, newSTRef,
                                          readSTRef)

main :: IO ()
main = do
    _:[s,t]:xs <- C.getContents <&> (C.lines >>> map C.words)
    let graph = M.fromList (map (\(name:c:_:ys) -> (name,(readInt c, ys))) xs)
    putStrLn $ runST $ do
        memo <- newSTRef M.empty
        dfs t graph memo S.empty s <&> maybe "SAFE" show

dfs :: C.ByteString -> M.Map C.ByteString (Int, [C.ByteString]) -> STRef s (M.Map C.ByteString (Maybe Int)) -> S.Set C.ByteString -> C.ByteString -> ST s (Maybe Int)
dfs t graph memo seen curr
    | curr == t          = pure (Just 0)
    | S.member curr seen = pure Nothing
    | otherwise          = do
        r <- readSTRef memo <&> M.lookup curr
        case r of
            Just  y -> pure y
            Nothing -> do
                let (c,ns) = graph M.! curr
                ret <- mapM (dfs t graph memo (S.insert curr seen)) ns <&> (sequence >>> fmap (maximum >>> (+c)))
                modifySTRef memo (M.insert curr ret)
                pure ret

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
