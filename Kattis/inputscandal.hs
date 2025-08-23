import qualified Data.ByteString.Char8 as C

main :: IO ()
main = do
    xs <- C.getContents
    print (length (C.lines xs))
    C.putStrLn xs
