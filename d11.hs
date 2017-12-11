import Data.Functor
import Data.List

type Direction = (Int,Int)

dir :: String -> Direction
dir "ne" = (1, -1)
dir "nw" = (-1, -1)
dir "n"  = (0, -2)
dir "se" = (1, 1)
dir "sw" = (-1, 1)
dir "s"  = (0, 2)

parse :: String -> [Direction]
parse = map dir . words . map f
  where
    f ',' = ' '
    f c   = c

distances :: (Int, Int) -> [Direction] -> [Int]
distances _ [] = []
distances (ax, ay) ((x,y):dirs) = (abs dx + abs dy) `div` 2 : distances (dx, dy) dirs
    where dx = ax + x
          dy = ay + y

main :: IO ()
main = do
    directions <- parse <$> readFile "d11.txt"
    let xs = distances (0, 0) directions
    print $ last xs
    print $ maximum xs

