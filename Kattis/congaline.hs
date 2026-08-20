import           Control.Arrow         ((>>>))
import           Control.Monad         (forM_, unless)
import           Control.Monad.ST      (ST, runST)
import           Data.Array            (listArray, (!))
import           Data.Bits             (xor)
import           Data.Bool             (bool)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust, fromMaybe)
import           Data.STRef.Strict     (STRef, modifySTRef, newSTRef, readSTRef)

data Person s = Person {
    name    :: C.ByteString,
    partner :: STRef s (Person s),
    front   :: Maybe (STRef s (Person s)),
    back    :: Maybe (STRef s (Person s))
}

main :: IO ()
main = do
    n':_:rest <- C.getContents <&> C.words
    let n = readInt n'
        (yelling,order) = runST $ do
            storage <- init rest
                & mapM (\x -> newSTRef (Person x undefined Nothing Nothing))
                <&> listArray (0,2*n-1)
            forM_ [0..2*n-1] $ \i -> do
                let front   = bool Nothing (Just (storage ! (i-1))) (i > 0)
                    back    = bool Nothing (Just (storage ! (i+1))) (i < 2*n-1)
                    partner = storage ! (i `xor` 1)
                modifySTRef (storage ! i) (\p -> p {partner=partner, front=front, back=back})
            conga (storage ! 0, storage ! (2*n-1), storage ! 0) [] (C.unpack (last rest))
    yelling <> [C.pack ""] <> order
        & C.unlines
        & C.putStr

conga :: (STRef s (Person s), STRef s (Person s), STRef s (Person s)) -> [C.ByteString] -> String -> ST s ([C.ByteString],[C.ByteString])
conga (start,end,curr) ret "" = do
    order <- backtrack start
    pure (reverse ret, order)
conga (start,end,curr) ret (x:xs) = case x of
    'F' -> do
        next <- readSTRef curr <&> (front >>> fromJust)
        conga (start,end,next) ret xs
    'B' -> do
        next <- readSTRef curr <&> (back >>> fromJust)
        conga (start,end,next) ret xs
    'R' | curr == end -> conga (start,end,start) ret xs
        | otherwise   -> do
            next <- readSTRef curr <&> (back >>> fromJust)
            reattach curr end
            let start' = bool start next (start == curr)
            conga (start',curr,next) ret xs
    'C' -> do
        Person {back,partner} <- readSTRef curr
        let next = fromMaybe start back
            start' = bool start next (start == curr)
            end' = bool end curr (end == partner)
        reattach curr partner
        conga (start',end',next) ret xs
    'P' -> do
        r <- readSTRef curr >>= (partner >>> readSTRef) <&> name
        conga (start,end,curr) (r : ret) xs

backtrack :: STRef s (Person s) -> ST s [C.ByteString]
backtrack ref = do
    Person {name,back} <- readSTRef ref
    (name:) <$> maybe (pure []) backtrack back

reattach :: STRef s (Person s) -> STRef s (Person s) -> ST s ()
reattach ref other = do
    per@Person {front,back} <- readSTRef ref
    unless (Just other == front) $ do
        maybe (pure ()) (`modifySTRef` \p -> p {back = back}) front
        maybe (pure ()) (`modifySTRef` \p -> p {front = front}) back

        Person {back} <- readSTRef other
        maybe (pure ()) (`modifySTRef` \p -> p {front = Just ref}) back
        modifySTRef other (\p -> p {back = Just ref})
        modifySTRef ref (\p -> p { front = Just other, back = back })

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
