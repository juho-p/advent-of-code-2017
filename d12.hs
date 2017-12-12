import Text.Parsec
import Text.Parsec.String

import qualified Data.Map as Map
import qualified Data.Set as Set

type Program = Int

data Connection = Connection Program [Program] deriving Show

connection :: Parser Connection
connection = do
    left   <- many1 digit
    _      <- string " <-> "
    rights <- many1 digit `sepBy` string ", "
    _      <- newline
    return $ Connection (read left) (map read rights)

type Graph = Map.Map Program (Set.Set Program)

graph :: [Connection] -> Graph
graph = foldr f Map.empty
  where
    f (Connection x xs) = Map.alter (Just . append xs) x
    append xs Nothing    = Set.fromList xs
    append xs (Just xs') = xs' `Set.union` Set.fromList xs

group :: Set.Set Program -> Program -> Graph -> Set.Set Program
group xs x g | Set.member x xs = xs
             | otherwise       = nodes
  where
    xs'       = Set.insert x xs
    neighbors = g Map.! x
    nodes     = Set.fold graphFold xs' neighbors
    graphFold p xs'' = group xs'' p g

groups :: Graph -> [Set.Set Program]
groups g = foldr f [] progs
  where
    progs = Map.keys g
    f p sets | any (Set.member p) sets = sets
             | otherwise               = group Set.empty p g : sets

main :: IO ()
main = do
    connections <- parseFromFile (many connection) "d12.txt"
    let g     = graph <$> connections
    let part1 = group Set.empty 0 <$> g
    print $ Set.size <$> part1
    let part2 = groups <$> g
    print $ length <$> part2
