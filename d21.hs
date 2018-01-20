import Text.Parsec
import Text.Parsec.String

import Data.Function
import Data.List

import qualified Data.Set as S
import qualified Data.Map.Strict as M

type Rule = ([[Bool]],[[Bool]])

rule :: Parser Rule
rule = do
    l <- image <* spaces <* string "=>" <* spaces
    r <- image <* newline
    return (l,r)
      where
        image :: Parser [[Bool]]
        image = row `sepBy` char '/'
        row = pixels <$> many1 (oneOf "#.")
        pixels = ((=='#') <$>)

transformations :: [[Bool]] -> S.Set [[Bool]]
transformations = tfs S.empty 
  where
    tfs :: S.Set [[Bool]] -> [[Bool]] -> S.Set [[Bool]]
    tfs acc img
      | S.member img acc = acc
      | otherwise = let
            acc' = S.insert img acc
            rots = take 3 $ iterate rotate (rotate img)
            ys = reverse img : (reverse <$> img) : rots
          in foldl tfs acc' ys
    -- yeah, pretty horrible solution...
    rotate (a : b : c : []) = [[c!!0,b!!0,a!!0],[c!!1,b!!1,a!!1],[c!!2,b!!2,a!!2]]
    rotate (a : b : []) = [[b!!0,a!!0],[b!!1,a!!1]]

mapping :: [Rule] -> M.Map [[Bool]] [[Bool]]
mapping [] = M.empty
mapping ((l, r):rules) =
    merge entries rest
      where
        entries = S.toList (transformations l)
        rest = mapping rules
        merge [] m = m
        merge (x:xs) m
            | M.member x m = error ("ambigous key " ++ show x)
            | otherwise = M.insert x r (merge xs m)

grouped :: Int -> [a] -> [[a]]
grouped n xs = map snd <$> groupBy ((==) `on` (\(i,_) -> i `div` n)) (zip [0..] xs)

split :: [[a]] -> [[[a]]]
split img = concatMap (transpose . (g <$>)) (g img)
  where
    n = if length img `mod` 2 == 0 then 2 else 3
    g = grouped n

merge :: [[[a]]] -> [[a]]
merge xs = concatMap (fmap concat . transpose) rows
    where rows = grouped (round (sqrt $ fromIntegral (length xs))) xs

process :: M.Map [[Bool]] [[Bool]] -> [[Bool]] -> [[Bool]]
process m img =
    let parts = split img
        parts' = map (m M.!) parts
        img' = merge parts'
    in img'

start = [[False,True,False],[False,False,True],[True,True,True]]

solve m =
    let steps = iterate (process m) start
        p1 = pixels (steps !! 5)
        p2 = pixels (steps !! 18)
        pixels img = sum (map (\x -> if x then 1 else 0) $ concat img)
    in (p1, p2)

main = do
    rules <- parseFromFile (many rule) "input/d21.txt"
    let m = mapping <$> rules
    print $ solve <$> m
