{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> head &&& (last >>> C.unpack >>> filter (/='+') >>> read))
        >>> terraform (0,-30,0)
        >>> putStrLn
    )

terraform :: (Int,Int,Int) -> [(C.ByteString,Int)] -> String
terraform (w,t,o) []                     | w >= 9 && t >= 8 && o >= 14 = "liveable"
                                         | otherwise                   = "not liveable"
terraform (w,t,o) (("ocean"      , d):xs) = terraform (w+d,t,o) xs
terraform (w,t,o) (("temperature", d):xs) = terraform (w,t+d,o) xs
terraform (w,t,o) (("oxygen"     , d):xs) = terraform (w,t,o+d) xs
