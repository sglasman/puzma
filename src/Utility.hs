module Utility where

import Data

allEqual :: (Eq a) => [a] -> Bool
allEqual [] = True
allEqual [x] = True
allEqual (x:xs) = (x == head xs) && allEqual xs

interleave :: [a] -> [a] -> [a]
interleave xs [] = xs
interleave [] ys = ys
interleave (x:xs) (y:ys) = [x, y] ++ interleave xs ys

coordArray :: Int -> Int -> [[GridCoord]]
coordArray height width = map (\x -> [(y, x) | y <- map normalizeCentral [1..width]]) $ map normalizeCentral [1..height]

coordArrayInterstitial :: Int -> Int -> [[GridCoord]]
coordArrayInterstitial height width = map (\x -> [(y, x) | y <- map normalizeInterstitial [0..width]]) $ map normalizeInterstitial [0..height]

normalizeCentral :: Int -> Int
normalizeCentral n = 2 * n - 1

normalizeInterstitial :: Int -> Int
normalizeInterstitial n = 2 * n