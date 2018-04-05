> {-# LANGUAGE LambdaCase #-}
> {-# LANGUAGE ViewPatterns #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE FunctionalDependencies #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE KindSignatures #-}
> module Sorting where
> import Data.Bool
> import qualified Data.ByteString.Lazy as B
> import Data.Foldable (foldrM)
> import Data.Maybe
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

> class Functor m => LogEq m a | m -> a where
>     (==.), (/=.)           :: a -> a -> m Bool

>     x /=. y               = fmap not (x ==. y)
>     x ==. y               = fmap not (x /=. y)
>     {-# MINIMAL (==.) | (/=.) #-}

> instance Ord a => LogEq (Writer (DS.Set (a, a))) a where
>   a ==. b = (a == b) <$ tell (DS.singleton (a, b))

> class (Monad m, LogEq m a) => LogOrd (m :: * -> *) a | m -> a where
>     logCompare              :: a -> a -> m Ordering
>     (<.), (<=.), (>.), (>=.) :: a -> a -> m Bool
>     logMax, logMin             :: a -> a -> m a

>     logCompare x y = judge <$> (x ==. y) <*> (x <=. y) where
>       judge True _ = EQ
>       judge _ True = LT
>       judge _ _ = GT

>     x <.  y = (\case { LT -> True;  _ -> False }) <$> logCompare x y
>     x <=. y = (\case { GT -> False; _ -> True }) <$> logCompare x y
>     x >.  y = (\case { GT -> True;  _ -> False }) <$> logCompare x y
>     x >=. y = (\case { LT -> False; _ -> True }) <$> logCompare x y

>     logMax x y = bool x y <$> (x <=. y)
>     logMin x y = bool y x <$> (x <=. y)
>     {-# MINIMAL logCompare | (<=.) #-}


> instance Ord a => LogOrd (Writer (DS.Set (a , a))) a where
>   logCompare a b = compare a b <$ tell (DS.singleton (a, b))

TODO: implement quick-select.

> quickMedian :: LogOrd w a => [a] -> w [a]
> quickMedian l = maybeToList <$> qselect (length l `div` 3) l

> qselect :: LogOrd w a => Int -> [a] -> w (Maybe a)
> qselect _ [] = return Nothing
> qselect n _ | n < 0 = return Nothing
> qselect 0 [x] = return (Just x)
> qselect n (middle -> Just (x, xs)) = do
> -- qselect n (x:xs) = do
>     cmp <- mapM (x <=.) xs
>     let res = zip cmp xs
>         low = map snd $ filter (not . fst) $ res
>         hi  = map snd $ filter fst $ res
>     case compare n (length low) of
>       LT -> qselect n low
>       EQ -> return (Just x)
>       GT -> qselect (n - length low) hi

> qsort :: LogOrd w a => [a] -> w [a]
> qsort [] = return []
> qsort (x:xs) = do
>     cmp <- mapM (x <=.) xs
>     let res = zip cmp xs
>         low = map snd $ filter (not . fst) $ res
>         hi  = map snd $ filter fst $ res
>     l <- qsort low
>     r <- qsort hi
>     return $ l ++ [x] ++ r

> middle :: [x] -> Maybe (x, [x])
> middle l = let (a,b) = splitAt (length l `div` 2) l
>            in (fmap.fmap) (a++) $ uncons b

> qsort' :: LogOrd w a => [a] -> w [a]
> qsort' (middle -> Just (x, xs)) = do
>     cmp <- mapM (x <=.) xs
>     let res = zip cmp xs
>         low = map snd $ filter (not . fst) $ res
>         hi  = map snd $ filter fst $ res
>     l <- qsort' low
>     r <- qsort' hi
>     return $ l ++ [x] ++ r
> qsort' x = return x


> merge :: LogOrd w a => [a] -> [a] -> w [a]
> merge [] r = return r
> merge l [] = return l
> merge (l:ls) (r:rs) = logCompare l r >>= \case
>     LT -> (l:) <$> merge ls (r:rs)
>     EQ -> ([l, r] ++) <$> merge ls rs
>     GT -> (r:) <$> merge (l:ls) rs

> insertM :: LogOrd w a => a -> [a] -> w [a]
> insertM x l = merge [x] l

> isort :: LogOrd w a => [a] -> w [a]
> isort = foldrM insertM []

> msort :: LogOrd w a => [a] -> w [a]
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

> minView :: LogOrd w a => [a] -> w (Maybe (a, [a]))
> minView [] = return Nothing
> minView (x:xs) = minView xs >>= \case
>     Nothing -> return $ Just (x, [])
>     Just (y, xs) -> logCompare x y >>= return . Just . \case
>         LT -> (x, y : xs)
>         EQ -> (x, y : xs)
>         GT -> (y, x : xs)

> selSort :: Ord a => [a] -> W a [a]
> selSort = minView >=> \case
>     Nothing -> return []
>     Just (x, xs) -> (x:) <$> selSort xs


     -- B.putStr $ imageToPng $ image $ DS.fromList [(200, 100), (100, 100)]
