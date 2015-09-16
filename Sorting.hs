import qualified Data.ByteString.Lazy as B
import Control.Arrow
import Control.Monad.Writer.Strict
import Codec.Picture
import Codec.Picture.Saving
import Control.Monad

import System.Random
import qualified Data.Set as DS
import Data.List (sort)
import Debug.Trace

image :: DS.Set (Int, Int) ->  DynamicImage
image set = ImageYF $ generateImage f size size where
    f x y | DS.member (x, y) set = 0.0
          -- | DS.member (y, x) set = 0.9
          | otherwise = 1.0
--    f x y = (sin (fromIntegral x / 20) / 2 + 0.5)
--          * (cos (fromIntegral y / 30) / 2 + 0.5)


size = 1000

list :: IO [Float]
list = replicateM size (randomIO :: IO Float)

le :: Ord a => a -> a -> Writer [(a, a)] Bool
le x y = do
    tell [(x, y)]
    return (x < y)

qsort :: Ord a => [a] -> Writer [(a, a)] [a]
qsort [] = return []
qsort (x:xs) = do
    cmp <- mapM (le x) xs
    let res = zip cmp xs
        low = map snd $ filter (not . fst) $ res
        hi  = map snd $ filter fst $ res
    l <- qsort low
    r <- qsort hi
    return $ l ++ [x] ++ r

shuffle :: [Float] -> [Int]
shuffle l = map snd $ sort $ zip l [0..]

main = do
    l <- list
    let ll = shuffle l
        ll' = zip ll [0..]
        w :: DS.Set (Int, Int)
        w = DS.map (snd***snd) $ DS.fromList $ execWriter $ qsort ll
    B.putStr $ imageToPng (image w)
    -- print w
    -- B.putStr $ imageToPng $ image $ DS.fromList [(200, 100), (100, 100)]
