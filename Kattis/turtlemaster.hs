{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((***), (>>>))
import           Control.Monad.ST      (ST, runST)
import           Data.Array.Base       (STUArray, UArray, assocs, bounds,
                                        listArray, newArray, readArray,
                                        writeArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Ix               (inRange)

main :: IO ()
main = do
    (board,instructions) <- C.getContents <&> (
                C.lines
            >>> splitAt 8
            >>> (C.concat >>> C.unpack >>> listArray ((1,1),(8,8)))
                    ***
                (head >>> C.unpack)
        )

    let diamond = runST $ do
            ice <- newArray ((1,1),(8,8)) False
            assocs board
                & filter (snd >>> (=='I'))
                & mapM_ (fst >>> flip (writeArray ice) True)
            turtle ice board 0 (8,1) instructions

    C.putStrLn $ if diamond
        then "Diamond!"
        else "Bug!"

turtle :: STUArray s (Int,Int) Bool -> UArray (Int,Int) Char -> Int -> (Int,Int) -> String -> ST s Bool
turtle ice board d pos [] = pure (board ! pos == 'D')
turtle ice board d pos (x:xs) = case x of
        'L'                -> turtle ice board (pred d `mod` 4) pos xs
        'R'                -> turtle ice board (succ d `mod` 4) pos xs
        _  | out           -> pure False
        'F' | C.elem front ".DT" -> turtle ice board d next xs
            | front == 'I' -> do
                i <- readArray ice next
                if i
                    then pure False
                    else turtle ice board d next xs
            | otherwise  -> pure False
        'X' | front == 'I' -> do
                i <- readArray ice next
                if i
                    then writeArray ice next False >> turtle ice board d pos xs
                    else pure False
            | otherwise -> pure False

    where
        next = dirs !! d +++ pos
        out = not (inRange (bounds board) next)
        front = board ! next

dirs :: [(Int,Int)]
dirs = [(0,1),(1,0),(0,-1),(-1,0)]

(+++) :: (Int,Int) -> (Int,Int) -> (Int,Int)
(+++) (a,b) (x,y) = (a+x, b+y)
