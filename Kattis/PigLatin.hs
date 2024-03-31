import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = C.getContents >>= (C.lines
        >>> mapM_ (C.words >>> map pigLatin >>> C.unwords >>> C.putStrLn)
    )

pigLatin :: C.ByteString -> C.ByteString
pigLatin xs = case C.findIndex (`elem` "aeiouy") xs of
    Just 0  -> xs <> C.pack "yay"
    Just i  -> C.concat [C.drop i xs, C.take i xs, C.pack "ay"]
    Nothing -> error "bruh"
