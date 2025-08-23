{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE StrictData    #-}
{-# LANGUAGE TupleSections #-}

import           Control.Applicative        (liftA2)
import           Control.Monad              (forM, replicateM)
import           Control.Monad.ST           (ST, runST)
import           Control.Monad.State.Strict (MonadState (get, put),
                                             MonadTrans (lift), State, StateT,
                                             evalState, execStateT)
import           Data.Array.Base            (STUArray (STUArray), listArray,
                                             listArrayST, listUArrayST)
import           Data.Array.ST              (STArray, STUArray, freeze,
                                             newArray, readArray, writeArray)
import           Data.Bool                  (bool)
import qualified Data.ByteString.Char8      as BC
import           Data.Char                  (isSpace, ord)
import           Data.Maybe                 (catMaybes, fromJust)
import           System.IO                  (isEOF)

data UF s = UF {
    repr  :: STUArray s Int Int,
    sizes :: STUArray s Int Int
}

newUF :: Int -> ST s (UF s)
newUF n = UF <$> listUArrayST (0,n-1) [0..] <*> newArray (0,n-1) 1

find :: UF s -> Int -> ST s Int
find uf u = do
    parent <- readArray (repr uf) u
    if parent == u
        then pure u
        else do
            grandparent <- find uf parent
            writeArray (repr uf) u grandparent
            pure grandparent

merge :: UF s -> Int -> Int -> ST s (Maybe (Int, Int))
merge uf u v = do
    p1 <- find uf u
    p2 <- find uf v
    if p1 == p2
        then pure Nothing
        else do
            s1 <- size uf p1
            s2 <- size uf p2
            if s1 < s2
                then do
                    writeArray (repr  uf) p1 p2
                    writeArray (sizes uf) p2 (s1 + s2)
                    pure (Just (p2, p1))
                else do
                    writeArray (repr  uf) p2 p1
                    writeArray (sizes uf) p1 (s1 + s2)
                    pure (Just (p1, p2))

size :: UF s -> Int -> ST s Int
size uf u = find uf u >>= readArray (sizes uf)

main :: IO ()
main = do
    inn <- readInts
    case inn of
        [] -> pure ()
        [n, m] -> do
            inputs <- fmap (map (map pred)) (replicateM m readInts)
            let outputs = runST $ do
                    uf <- newUF n
                    whear <- listUArrayST (0,n-1) [0..]
                    summm <- listArrayST (0,n-1) (map (1,) [1..])
                    forM inputs $ (\case
                            [0, p, q] -> do
                                p' <- readArray whear p
                                q' <- readArray whear q
                                result <- merge uf p' q'
                                case result of
                                    Nothing -> pure Nothing
                                    Just (newRoot, oldRoot) -> do
                                        s1 <- readArray summm newRoot
                                        s2 <- readArray summm oldRoot
                                        writeArray summm newRoot (s1 +++ s2)
                                        pure Nothing
                            [1, p, q] -> do
                                p' <- readArray whear p >>= find uf
                                q' <- readArray whear q >>= find uf
                                sp <- readArray summm p'
                                writeArray summm p' (sp +++ (-1, -fromIntegral p-1))
                                sq <- readArray summm q'
                                writeArray summm q' (sq +++ (1,fromIntegral p+1))
                                writeArray whear p q'
                                pure Nothing
                            [2, p]    -> do
                                p' <- readArray whear p >>= find uf
                                Just <$> readArray summm p'
                            _         -> error "bruh"
                        )

            mapM_ (\(size, summ) -> putStrLn (show size ++ " " ++ show summ)) $ catMaybes outputs

            main

eatChar :: State BC.ByteString (Maybe Char)
eatChar = do
    m <- fmap BC.uncons get
    case m of
        Nothing -> return Nothing
        Just (x, xs) -> do
            put xs
            return $ Just x

eatInt :: State BC.ByteString (Maybe Int)
eatInt = do
    m <- eatChar
    case m of
        Nothing -> return Nothing
        Just '-' -> do
            positive <- fmap fromJust eatInt
            return $ Just (negate positive)
        Just c -> do
            fmap Just $ execStateT inner $ ord c - ord '0'
    where
        inner :: StateT Int (State BC.ByteString) ()
        inner = do
            m <- lift eatChar
            case m of
                Nothing -> return ()
                Just c -> do
                    if isSpace c then
                        return ()
                    else do
                        was <- get
                        put $ 10 * was + ord c - ord '0'
                        inner

eatInts :: State BC.ByteString [Int]
eatInts = do
    m <- eatInt
    case m of
        Nothing -> return []
        Just x -> do
            xs <- eatInts
            return (x:xs)

readInts :: IO [Int]
readInts = isEOF >>= bool (evalState eatInts <$> BC.getLine) (pure [])

(+++) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
(+++) (x,y) (a,b) = (x+a, y+b)

