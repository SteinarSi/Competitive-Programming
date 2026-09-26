import           Control.Arrow         ((>>>))
import           Control.Monad         (replicateM)
import           Data.Array            (listArray, (!))
import           Data.Array.IO         (IOUArray, newArray, readArray,
                                        writeArray)
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)
import           Data.Word             (Word8)

data Instruction =
      CONST Word8 Word8
    | ADD Word8 Word8
    | SUB Word8 Word8
    | JNZ Word8 Word8
    | INPUT Word8
    | OUTPUT Word8
    | HALT
    deriving (Read,Show)

main :: IO ()
main = do
    n <- C.getLine <&> readWord8
    code <- replicateM (fromIntegral n) (C.getLine <&> (C.unpack >>> read)) <&> listArray (0,n-1)
    regs <- newArray (0,255) 0 :: IO (IOUArray Word8 Word8)

    let
        run :: Word8 -> IO ()
        run i = case code ! i of
            CONST x y -> writeArray regs y x >> run (i+1)
            ADD x y -> do
                x' <- readArray regs x
                y' <- readArray regs y
                writeArray regs y (x'+y')
                run  (i+1)
            SUB x y -> do
                x' <- readArray regs x
                y' <- readArray regs y
                writeArray regs y (y'-x')
                run  (i+1)
            JNZ x y -> do
                z <- readArray regs x
                if z /= 0
                    then run y
                    else run (i+1)
            INPUT x -> do
                C.getLine >>= (readWord8 >>> writeArray regs x)
                run (i+1)
            OUTPUT x -> do
                readArray regs x >>= print
                run (i+1)
            HALT -> pure ()

    run 0

readWord8 :: C.ByteString -> Word8
readWord8 = C.readWord8 >>> fromJust >>> fst
