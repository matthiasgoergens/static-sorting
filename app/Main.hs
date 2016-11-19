{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Codec.Picture
import Codec.Picture.Saving

import Control.Arrow
import Control.Monad
import Control.Monad.Writer.Strict

import qualified Data.ByteString.Lazy as B
import Data.Foldable (foldrM)
import Data.List (sort, uncons)
import qualified Data.Set as DS

import Debug.Trace

import Development.Shake

import System.IO
import System.Random

import Sorting

main = do
    (l :: [Int]) <- shuffle <$> list
    let perSort sortName fname' f q' = do
            let fname = sortName ++ fname' ++ show size ++ ".png"
                q = DS.map f q'
            putStrLn fname
            withFile fname WriteMode $ \h -> B.hPutStr h $ imageToPng (image q)

--    perSort "msortSorted" msort [0..size-1]
--    perSortOrig "msortSorted" msort [0.. size-1]
    -- perSort "qsortMiddleSorted" qsort' [0..size-1]
    -- perSortOrig "qsortMiddleSorted" qsort' [0..size-1]
    let f name sort = do
            let q :: DS.Set ((Int, Int), (Int, Int))
                q = execWriter $ sort (zip l [0..])
            perSort name ""      (fst *** fst) q
            perSort name "OrigX" (snd *** fst) q
            perSort name "OrigY" (fst *** snd) q
            perSort name "Orig"  (snd *** snd) q
    -- f "insertion-sort" isort
    -- f "quick-sort" qsort'
    -- f "merge-sort" msort
    -- f "selection-sort" selSort

    let f name sort = do
            let q :: DS.Set ((Int, Int), (Int, Int))
                q = execWriter $ sort (zip [0..size-1] [0..])
            perSort name "Sorted"      (fst *** fst) q
            perSort name "SortedOrigX" (snd *** fst) q
            perSort name "SortedOrigY" (fst *** snd) q
            perSort name "SortedOrig"  (snd *** snd) q
    f "insertion-sort" isort
    f "quick-sort" qsort'
    f "merge-sort" msort
    f "selection-sort" selSort

    -- perSort "msortRevSorted" msort $ reverse [0..size-1]
--    perSortOrig "insert" isort l
--    perSort "insert" isort l
    -- print w

