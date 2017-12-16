import Data.List
import Data.Maybe

import Text.Parsec
import Text.Parsec.String

data Layer = Layer Int Int deriving Show

caught :: Int -> Layer -> Bool
caught delay (Layer tick depth) = ((tick + delay) `mod` len) == 0
  where
    len | depth == 0 = 1
        | otherwise  = (depth - 1) * 2

layer :: Parser Layer
layer = do
    index <- integer <* char ':' <* spaces
    depth <- integer <* spaces
    return $ Layer index depth
    where integer = read <$> many digit

severity :: Layer -> Int
severity x@(Layer i d)
    | caught 0 x = i * d
    | otherwise = 0

minDelay :: [Layer] -> Int
minDelay layers = fromMaybe 0 $ find (not . anyCaught) [0..]
    where anyCaught delay = any (caught delay) layers

main :: IO ()
main = do
    layers <- parseFromFile (many layer) "d13.txt"
    let part1 = sum . map severity <$> layers
    print part1
    let part2 = minDelay <$> layers
    print part2
