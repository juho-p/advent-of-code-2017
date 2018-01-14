{-# LANGUAGE BangPatterns #-}

import qualified Data.Set as Set
import Data.List
import qualified Data.Vector as V

import Text.Parsec
import Text.Parsec.String

type Component = (Int,Int)

portsParser :: Parser [Component]
portsParser = many port
    where
      port = do
        a <- num <* char '/'
        b <- num <* spaces
        return (a, b)
      num = read <$> many1 digit

solve :: [Component] -> (Int, Int)
solve cs = (part1, part2)
  where
    part1 = maximum strengths
    part2 = f (zip (length <$> goodBridges) (strength <$> goodBridges)) (0, 0)
      where
        f :: [(Int,Int)] -> (Int,Int) -> Int
        f (sc@(!l,!s):xs) sc0@(!l0,!s0)
            | l > l0 = f xs sc
            | l == l0 && s > s0 = f xs sc
            | otherwise = f xs sc0
        f [] x = snd x
    strengths = strength <$> goodBridges
    strength !xs = sum $ map (uncurry (+)) xs
    goodBridges = bridges 0 Set.empty
    cs' = V.indexed (V.fromList $ sortOn (uncurry (+)) cs)
    bridges :: Int -> Set.Set Int -> [[Component]]
    bridges !p used = nexts
      where
        valids = V.filter (\(idx,(a,b)) -> not (Set.member idx used) && (a == p || b == p)) cs'
        nexts :: [[Component]]
        nexts = nexts' `concatMap` valids
        nexts' :: (Int,Component) -> [[Component]]
        nexts' (i,x@(a,b)) = let
            y = (x:) <$> bridges (if a == p then b else a) (Set.insert i used)
          in if null y then [[x]] else y

main :: IO ()
main = do
    ports <- parseFromFile portsParser "input/d24.txt"
    --print $ take 10 . bridges 0 <$> ports'
    print $ solve <$> ports
