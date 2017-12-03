import Data.List
import Data.Maybe
import qualified Data.Map as Map

input = 347991 :: Int

type Vec2 = (Int,Int)

nextDir :: Vec2 -> Vec2
nextDir (x,y) = (y, -x)

add (x1,y1) (x2,y2) = (x1+x2, y1+y2)

side :: Int -> Vec2 -> Vec2 -> ([Vec2], Vec2)
side n dir from = (side, end)
    where side = take (sideCount) (iterate (add dir) from)
          end = (
            (fst from) + (sideCount * fst dir),
            (snd from) + (sideCount * snd dir))
          sideCount = (n `div` 2) + 1

spiral :: [Vec2]
spiral =
    let directions = iterate nextDir (1,0)
        nextSides = map (\(a,b) -> side a b) (zip [0..] directions)
        nextIteration ((f:fs), (side, v)) = (fs, f v)
        iterations = iterate nextIteration (tail nextSides, (head nextSides) (0,0))
    in
        concatMap (\(_, (s, _)) -> s) iterations

distance n = abs dx + abs dy
    where (dx, dy) = spiral !! (n-1)

neighbors :: Vec2 -> [Vec2]
neighbors (x,y) = [(x+dx, y+dy) |
    dx <- [-1..1],
    dy <- [-1..1],
    dx /= 0 || dy /= 0 ]

sumSpiral :: [Int]
sumSpiral = 1 : (sumSpiral' (Map.singleton (0,0) 1) (drop 1 spiral))

sumSpiral' :: Map.Map Vec2 Int -> [Vec2] -> [Int]
sumSpiral' prevValues (place:places) =
    let newValue = sum (mapMaybe (`Map.lookup` prevValues) (neighbors place))
        newValues = Map.insert place newValue prevValues
    in
        newValue : (sumSpiral' newValues places)

sumSpiralValue limit = find (>limit) sumSpiral

main = do
    print $ distance input
    print $ sumSpiralValue input
