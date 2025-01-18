import           Control.Arrow         ((***), (>>>))
import           Control.Monad         (forM)
import           Control.Monad.ST      (ST, runST)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.Map              as M
import           Data.Maybe            (fromJust)
import           Data.STRef.Strict     (STRef, modifySTRef, newSTRef, readSTRef)

main :: IO ()
main = do
    [n,_]:[founder]:rest <- C.getContents <&> (C.lines >>> map C.words)
    let (heritage,claimers) = splitAt (readInt n) rest
            & (map (\(c:d:m:_) -> (c,(d,m))) >>> M.fromList)
                ***
              map head
    C.putStrLn $ runST $ do
            memo <- newSTRef (M.singleton founder 1)
            forM claimers (\name -> claim heritage memo name <&> (,name)) <&> (maximum >>> snd)

claim :: M.Map C.ByteString (C.ByteString,C.ByteString) -> STRef s (M.Map C.ByteString Double) -> C.ByteString -> ST s Double
claim heritage memo name = do
    mem <- readSTRef memo
    case M.lookup name mem of
        Just c  -> pure c
        Nothing -> case M.lookup name heritage of
            Nothing -> pure 0
            Just (dad,mum) -> do
                d <- claim heritage memo dad
                m <- claim heritage memo mum
                let c = (d+m) / 2
                modifySTRef memo (M.insert name c)
                pure c

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
