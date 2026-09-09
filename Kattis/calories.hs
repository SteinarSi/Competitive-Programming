{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Arrow         ((***), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Either           (partitionEithers)
import           Data.Function         ((&))
import           Data.Maybe            (fromJust)


data Unit = Grams | Percent | Calories
    deriving (Show,Eq)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> parse
        >>> map (map (interpret >>> \(x:xs) -> (x,sum xs))
            >>> foldl' (\(x,xs) (y,ys) -> (x+y,xs+ys)) (0,0)
            >>> (\(x,xs) -> show (round (100 * x / (x+xs))) <> "%"))
        >>> unlines
        >>> putStr
    )

interpret :: [(Double,Unit)] -> [Double]
interpret xs = zipWith (\c -> \case (x,Grams) -> c*x; (x,Calories) -> x; (x,Percent) -> totalCalories * x / 100) calsPerGram xs
  where
    (percent,calories) = xs
        & zipWith (\c -> \case (x,Grams) -> Right (c*x); (x,Calories) -> Right x; (x,Percent) -> Left x) calsPerGram
        & partitionEithers
        & sum *** sum
    totalCalories = calories * 100 / (100 - percent)

calsPerGram :: [Double]
calsPerGram = [9, 4, 4, 4, 7]

parse :: [C.ByteString] -> [[[(Double,Unit)]]]
parse []    = []
parse ["-"] = []
parse xss   = amounts : parse zss
  where
    (yss,_:zss) = span (/="-") xss
    amounts = map (C.words >>> map (C.readInt >>> fromJust >>> fromIntegral *** (\case "g" -> Grams; "%" -> Percent; "C" -> Calories))) yss
