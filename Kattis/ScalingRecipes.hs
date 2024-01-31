import           Control.Monad (forM_, replicateM)
import           Data.List     (find)
import           Data.Maybe    (fromJust)

main :: IO ()
main = do
    n <- fmap read getLine
    forM_ [1..n] $ \i -> do
        putStrLn ("Recipe # " ++ show i)
        [r, p, d] <- fmap (map read . words) getLine
        ingredients <- fmap (map ((\[name,weight,percent] -> (name, (read weight, read percent))) . words)) (replicateM (round r) getLine)

        let scale = d / p
            (mainWeight, mainPercent) = snd $ fromJust $ find ((100.0==) . snd . snd) ingredients
            scaledWeight = mainWeight * scale

        mapM_ (\(name, (weight, percent)) -> putStrLn (name ++ " " ++ show (scaledWeight * percent / 100))) ingredients

        putStrLn (replicate 40 '-')
