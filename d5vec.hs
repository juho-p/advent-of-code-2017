{-# LANGUAGE BangPatterns #-}

-- super slow (proving that updating single item in Data.Vector is just sad)

import qualified Data.Vector.Unboxed as V

parse :: String -> [Int]
parse = map read . words

type Executor = V.Vector Int -> Int -> Maybe (Int, V.Vector Int)

exec :: (Int -> Int) -> Executor
exec f v i = case v V.!? i of
    Nothing -> Nothing
    Just x -> Just (i + x, v V.// [(i, f x)])

simpleExec :: Executor
simpleExec = exec (+1)

advancedExec :: Executor
advancedExec = exec (\x -> if x >= 3 then x - 1 else x + 1)

steps :: Executor -> [Int] -> Int
steps f offsets = steps' f 0 0 $ V.fromList offsets

steps' :: Executor -> Int -> Int -> V.Vector Int -> Int
steps' f !cnt !index v = case f v index of
    Nothing -> cnt
    Just (index', v') -> steps' f (cnt + 1) index' v'

main :: IO ()
main = do
    input <- readFile "input/d5.txt"
    let offsets = parse input
    print $ steps simpleExec offsets
    --print $ steps advancedExec offsets
