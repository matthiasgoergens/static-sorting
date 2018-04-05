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
import Data.Hashable
import Data.List (sort, uncons)
import Data.List.Extra (splitOn, trim)
import qualified Data.Set as DS

import Debug.Trace

import Development.Shake
import Development.Shake.FilePath

import System.IO
import System.Random
import System.Random.Shuffle

import Sorting

type Seed = Int

inputList :: Seed -> Int -> [Int]
inputList seed size = shuffle' [0..size-1] size (mkStdGen seed)

whichAlgo :: Ord a => String -> ([a] -> Writer (DS.Set (a, a)) [a])
whichAlgo = \case
  "insert" -> isort
  "quick" -> qsort'
  "quickselect" -> quickMedian
  "merge" -> msort
  "selection" -> selSort
  x -> error $ "Don't know about algorithm: " ++ x
presentation = \case
  "xy" -> (fst *** fst)
  "-y" -> (snd *** fst)
  "x-" -> (fst *** snd)
  "--" -> (snd *** snd)
  x -> error $ "Don't know about axes: " ++ x

main = shakeArgs shakeOptions { shakeThreads = 0, shakeChange = ChangeModtimeAndDigestInput } $ do
  -- TODO: get Shake's FilePattern module.
  action $ do
    need . filter (not . null) . map trim =<< readFileLines "list"
  "images/*_*_*_*.png" %> \filename -> do
    let [whichAlgo -> algo, input, presentation -> options, read -> size]
          = splitOn "_" $ takeFileName $ dropExtension filename
    l <- case input of
        "presorted" -> return [0..size-1]
        "random" -> do
          seed <- hash <$> readFile' "seed"
          return $ inputList seed size
    -- Consider switching to Data.Sequence for the writer.
    let comparisons = DS.map options $ execWriter $ algo (zip l [0..])

    traced ("Writing image") $ withFile filename WriteMode $ \h -> B.hPutStr h $ imageToPng (image size comparisons)
