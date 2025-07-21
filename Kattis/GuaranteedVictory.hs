import           Control.Arrow         ((&&&), (***), (>>>))
import           Control.Monad         (filterM)
import           Control.Monad.ST      (ST, runST)
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromJust)
import qualified Data.Set              as S
import           Data.STRef.Strict     (STRef, modifySTRef, newSTRef, readSTRef)

main :: IO ()
main = do
    nr:rest <- C.getContents <&> C.lines
    splitAt (readInt nr) rest
        & S.fromList *** (zip [0..]
            >>> filter (fst >>> odd)
            >>> map (snd >>> C.words >>> (\(x:xs) -> (x,xs)))
            >>> M.fromList
        )
        & uncurry solve
        & putStrLn

solve :: S.Set C.ByteString -> M.Map C.ByteString [C.ByteString] -> String
solve parts prints = runST $ do
    dp <- newSTRef M.empty
    let t = M.keys prints
    imp <- filterM (possible dp >>> (<&> not)) (M.keys prints) <&> length
    if imp == 0
        then pure "GUARANTEED VICTORY"
        else pure (show imp)
  where
    possible :: STRef s (M.Map C.ByteString Bool) -> C.ByteString -> ST s Bool
    possible dp u | S.member u parts = pure True
                  | otherwise        = do
        seen <- readSTRef dp <&> M.lookup u
        case seen of
            Just  s -> pure s
            Nothing -> do
                modifySTRef dp (M.insert u False)
                poss <- allM (possible dp) (prints M.! u)
                modifySTRef dp (M.insert u poss)
                pure poss

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM p []     = pure True
allM p (x:xs) = p x >>= bool (pure False) (allM p xs)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
