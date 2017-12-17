{-# LANGUAGE BangPatterns #-}

import qualified Data.Sequence as S
import Data.List

start :: (Int, S.Seq Int)
start = (0, S.singleton 0)

numSteps = 301

step :: (Int, S.Seq Int) -> (Int, S.Seq Int)
step (!pos, !values) = (pos', (left S.|> len) S.>< right)
  where
    pos'  = ((pos + numSteps) `mod` len) + 1
    len   = S.length values
    left  = S.take pos' values
    right = S.drop pos' values

part1 = foldl' (\b _ -> step b) start [1..2017]

part2 = after <$> S.elemIndexL 0 xs
    where (_, xs) = until ((>50000000) . S.length . snd) step start
          after i = S.index xs (i+1)

main :: IO ()
main = do
    let (i, xs) = part1
    print $ S.index xs (i + 1)

    print part2
