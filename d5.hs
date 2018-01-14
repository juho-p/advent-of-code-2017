{-# LANGUAGE BangPatterns #-}

import qualified Data.Map as Map

parse :: String -> [Int]
parse = map read . words

type Executor = Map.Map Int Int -> Int -> Maybe (Int, Map.Map Int Int)

exec :: (Int -> Int) -> Executor
exec f m i = case Map.lookup i m of
    Nothing -> Nothing
    Just x -> Just (i + x, Map.insert i (f x) m)

simpleExec :: Executor
simpleExec = exec (+1)

advancedExec :: Executor
advancedExec = exec (\x -> if x >= 3 then x - 1 else x + 1)

steps :: Executor -> [Int] -> Int
steps f offsets = steps' f 0 0 $ Map.fromList (zip [0..] offsets)

steps' :: Executor -> Int -> Int -> Map.Map Int Int -> Int
steps' !f !cnt !index !m = case f m index of
    Nothing -> cnt
    Just (index', m') -> steps' f (cnt + 1) index' m'

main :: IO ()
main = do
    input <- readFile "input/d5.txt"
    let offsets = parse input
    print $ steps simpleExec offsets
    print $ steps advancedExec offsets
