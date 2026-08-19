import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Maybe            (fromJust)
import           System.IO             (hFlush, stdout)

main :: IO ()
main = do
    n <- readInt <$> C.getLine
    hunt n (1,n)

hunt :: Int -> (Int,Int) -> IO ()
hunt n (f,t)
    | f == t = catch f
    | f+2 >= t = do
        res <- query (f,f) (t,t)
        case res of
            (True , False) -> catch f
            (False, False) -> catch (f+1)
            (False, True ) -> catch t
            (True , True ) -> catch t
    | otherwise = do
        let (q,r) = quotRem (t+1-f) 4
            [a,b,c] = [q,q,q]
                & zipWith (+) (replicate (r-1) 1 <> repeat 0)
                & scanl1 (+)
                & map (+(f-1))
        res <- query (f,b) (a+1,c)
        case res of
            (True , False) -> observe (f,a)
            (True , True ) -> observe (a+1,b)
            (False, True ) -> observe (b+1,c)
            (False, False) -> observe (c+1,t)
  where
    observe ::(Int,Int) -> IO ()
    observe (f,t)
        | f==t      = catch f
        | otherwise = hunt n (max 1 (f-1), min n (t+1))

query :: (Int,Int) -> (Int,Int) -> IO (Bool,Bool)
query (a,b) (x,y) = do
    putStrLn (unwords ("Q" : map show [a,b,x,y]))
    hFlush stdout
    res <- C.getLine
    pure (C.head res == '1', C.last res == '1')

catch :: Int -> IO ()
catch x = do
    putStrLn ("A " <> show x)
    hFlush stdout

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
