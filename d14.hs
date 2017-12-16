import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as VB
import qualified Data.Set as Set
import qualified Data.Map as Map

import Text.Printf
import Data.List
import Data.Maybe
import Data.Graph
import Knothash

input :: String
input = "ljoxqyyw"

hash :: String -> String
hash s = concatMap (printf "%08b") (knothash $ toLengths s)

bits :: VB.Vector (V.Vector Bool)
bits = row <$> VB.fromList [0..127]
    where rowHash x = map (=='1') $ hash (input ++ "-" ++ show x)
          row = V.fromList . rowHash

bit :: (Int, Int) -> Bool
bit (x, y) = fromMaybe False perhaps
    where perhaps = (bits VB.!? y) >>= (V.!? x)

part1 :: Int
part1 = sum rows
    where rows = map row [0..127]
          row x = length (elemIndices True (V.toList $ bits VB.! x))
graph :: Graph
graph =
    let
        rows = [0..127]
        cols = [0..127]
        allPlaces = do
            y <- rows
            x <- cols
            return (x, y)
        nodes = filter bit allPlaces
        neighbors (x, y) = [(x-1, y), (x, y-1), (x+1, y), (x, y+1)]
        graphEdges = map (\n -> ((), n, filter bit $ neighbors n)) nodes
        (g, _, _) = graphFromEdges graphEdges
    in g

main :: IO ()
main = putStrLn $ show part1 ++ "\n" ++ show (length $ dff graph)
