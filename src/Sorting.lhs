> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE ViewPatterns #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> module Sorting where
> import qualified Data.ByteString.Lazy as B
> import Data.Foldable (foldrM)
> import System.IO
> import Control.Arrow
> import Control.Monad.Writer.Strict
> import Codec.Picture
> import Codec.Picture.Saving
> import Control.Monad

> import System.Random
> import qualified Data.Set as DS
> import Data.List (sort, uncons)
> import Debug.Trace

> image :: Int -> DS.Set (Int, Int) ->  DynamicImage
> image size set = ImageYF $ generateImage f size size where
>     f x y | DS.member (x, y) set = 0.0
>           -- | DS.member (y, x) set = 0.9
>           | otherwise = 1.0
> --    f x y = (sin (fromIntegral x / 20) / 2 + 0.5)
> --          * (cos (fromIntegral y / 30) / 2 + 0.5)


> le :: Ord a => a -> a -> Writer (DS.Set (a, a)) Bool
> le x y = do
>     tell $ DS.singleton (x, y)
>     return (x < y)

> cmp :: Ord a => a -> a -> Writer (DS.Set (a, a)) Ordering
> cmp x y = do
>     tell $ DS.singleton (x, y)
>     return $ compare x y

> qsort :: Ord a => [a] -> Writer (DS.Set (a, a)) [a]
> qsort [] = return []
> qsort (x:xs) = do
>     cmp <- mapM (le x) xs
>     let res = zip cmp xs
>         low = map snd $ filter (not . fst) $ res
>         hi  = map snd $ filter fst $ res
>     l <- qsort low
>     r <- qsort hi
>     return $ l ++ [x] ++ r

> middle :: [x] -> Maybe (x, [x])
> middle l = let (a,b) = splitAt (length l `div` 2) l
>            in (fmap.fmap) (a++) $ uncons b

> qsort' :: Ord a => [a] -> Writer (DS.Set (a, a)) [a]
> qsort' (middle -> Just (x, xs)) = do
>     cmp <- mapM (le x) xs
>     let res = zip cmp xs
>         low = map snd $ filter (not . fst) $ res
>         hi  = map snd $ filter fst $ res
>     l <- qsort' low
>     r <- qsort' hi
>     return $ l ++ [x] ++ r
> qsort' x = return x


> merge :: Ord a => [a] -> [a] -> Writer (DS.Set (a, a)) [a]
> merge [] r = return r
> merge l [] = return l
> merge (l:ls) (r:rs) = cmp l r >>= \case
>     LT -> (l:) <$> merge ls (r:rs)
>     EQ -> ([l, r] ++) <$> merge ls rs
>     GT -> (r:) <$> merge (l:ls) rs

> insertM :: Ord a => a -> [a] -> Writer (DS.Set (a, a)) [a]
> insertM x l = merge [x] l

> isort :: Ord a => [a] -> Writer (DS.Set (a, a)) [a]
> isort = foldrM insertM []

> msort :: Ord a => [a] -> Writer (DS.Set (a, a)) [a]
> msort [] = return []
> msort [x] = return [x]
> msort l = iter (map return l) where
>     pair (x:y:xs) = do
>         (:) <$> merge x y <*> pair xs
>     pair l = return l

>     iter [] = return []
>     iter [x] = return x
>     iter l = iter =<< pair l

> type W a b = Writer (DS.Set (a, a)) b

> minView :: Ord a => [a] -> W a (Maybe (a, [a]))
> minView [] = return Nothing
> minView (x:xs) = minView xs >>= \case
>     Nothing -> return $ Just (x, [])
>     Just (y, xs) -> cmp x y >>= return . Just . \case
>         LT -> (x, y : xs)
>         EQ -> (x, y : xs)
>         GT -> (y, x : xs)

> selSort :: Ord a => [a] -> W a [a]
> selSort = minView >=> \case
>     Nothing -> return []
>     Just (x, xs) -> (x:) <$> selSort xs


     -- B.putStr $ imageToPng $ image $ DS.fromList [(200, 100), (100, 100)]
