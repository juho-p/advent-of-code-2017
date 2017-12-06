{-# LANGUAGE BangPatterns #-}

import qualified Data.Map as Map

type Banks = Map.Map Int Int

distribute :: Int -> Int -> Int -> Banks -> Banks
distribute n blocks start = Map.mapWithKey newValue
  where
    newValue i x = x + if extraBlock i then wholeBlocks + 1 else wholeBlocks
    extraBlock i = ((i - start + n) `mod` n) < r
    (wholeBlocks, r) = blocks `divMod` n

biggestValueWithKey :: Banks -> (Int, Int)
biggestValueWithKey = Map.foldWithKey f (-1, -1)
  where
    f k a (k', a') | a > a'    = (k, a)
                   | a == a'   = (min k k', a)
                   | otherwise = (k', a')

redistribute :: Int -> Banks -> Banks
redistribute n banks = distribute n biggestValue startKey clearMap
  where
    startKey                   = (biggestKey + 1) `mod` n
    clearMap                   = Map.insert biggestKey 0 banks
    (biggestKey, biggestValue) = biggestValueWithKey banks

--states :: [Int] -> [Banks]
--states xs = states' (length xs) (Map.fromList (zip [0..] xs))
--
--states' :: Int -> Banks -> [Banks]
--states' n banks = banks' : states' n banks'
--    where banks' = redistribute n banks

cycles :: [Int] -> (Int, Int)
cycles xs = cycles' (length xs) 0 Map.empty (Map.fromList (zip [0 ..] xs))

cycles' :: Int -> Int -> Map.Map Banks Int -> Banks -> (Int, Int)
cycles' !n !acc !seen !banks = case Map.lookup banks seen of
    Just x -> (acc, acc - x)
    Nothing ->
        cycles' n (acc + 1) (Map.insert banks acc seen) (redistribute n banks)

parse :: String -> [Int]
parse = map read . words

main :: IO ()
main = do
    input <- readFile "d6.txt"
    let xs = parse input
    print $ cycles xs
