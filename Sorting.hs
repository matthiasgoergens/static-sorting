{-# LANGUAGE LambdaCase #-}
import qualified Data.ByteString.Lazy as B
import System.IO
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


size = 2000

list :: IO [Float]
list = replicateM size (randomIO :: IO Float)

le :: Ord a => a -> a -> Writer (DS.Set (a, a)) Bool
le x y = do
    tell $ DS.singleton (x, y)
    return (x < y)

cmp :: Ord a => a -> a -> Writer (DS.Set (a, a)) Ordering
cmp x y = do
    tell $ DS.singleton (x, y)
    return $ compare x y

qsort :: Ord a => [a] -> Writer (DS.Set (a, a)) [a]
qsort [] = return []
qsort (x:xs) = do
    cmp <- mapM (le x) xs
    let res = zip cmp xs
        low = map snd $ filter (not . fst) $ res
        hi  = map snd $ filter fst $ res
    l <- qsort low
    r <- qsort hi
    return $ l ++ [x] ++ r

merge :: Ord a => [a] -> [a] -> Writer (DS.Set (a, a)) [a]
merge [] r = return r
merge l [] = return l
merge (l:ls) (r:rs) = cmp l r >>= \case
    LT -> (l:) <$> merge ls (r:rs)
    EQ -> ([l, r] ++) <$> merge ls rs
    GT -> (r:) <$> merge (l:ls) rs

msort :: Ord a => [a] -> Writer (DS.Set (a, a)) [a]
msort [] = return []
msort [x] = return [x]
msort l = iter (map return l) where
    pair (x:y:xs) = do
        (:) <$> merge x y <*> pair xs
    pair l = return l

    iter [] = return []
    iter [x] = return x
    iter l = iter =<< pair l


shuffle :: [Float] -> [Int]
shuffle l = map snd $ sort $ zip l [0..]

main = do
    l <- shuffle <$> list
    let perSort sortName sort l = do
            let q :: DS.Set (Int, Int)
                q = execWriter $ sort l
                fname = sortName ++ show size ++ ".png"
            putStrLn fname
            withFile fname WriteMode $ \h -> B.hPutStr h $ imageToPng (image q)
    let perSortOrig sortName sort l = do
            let q :: DS.Set (Int, Int)
                q = DS.map (snd *** snd) $ execWriter $ sort $ zip l [0..]
                fname = sortName ++ "Orig" ++ show size ++ ".png"
            putStrLn fname
            withFile fname WriteMode $ \h -> B.hPutStr h $ imageToPng (image q)

    -- perSort "qsort" qsort l
    perSort "msort" msort l
    perSortOrig "msort" msort l
    perSort "qsort" qsort l
    perSortOrig "qsort" qsort l
    -- perSort "msortRevSorted" msort $ reverse [0..size-1]
    -- print w
    -- B.putStr $ imageToPng $ image $ DS.fromList [(200, 100), (100, 100)]
