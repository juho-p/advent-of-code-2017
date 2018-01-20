{-# LANGUAGE BangPatterns #-}

import Text.Parsec
import Text.Parsec.String

import qualified Data.Set as S
import qualified Data.Map as M

parser :: Parser (S.Set (Int,Int), Int, Int)
parser = collect <$> many row
  where
    row = map (=='#') <$> (many (oneOf ".#") <* newline)
    collect rows =
        let points = do
                (y,r) <- zip [0..] rows
                (x,_) <- filter snd (zip [0..] r)
                return (x,y)
            h = length rows
            w = length $ head rows
        in (S.fromList points, w, h)

add (a,b) (c,d) = (a+c,b+d)
right (x,y) = (-y, x)
left (x,y) = (y, -x)

move (!c, !p, !d, !i) = (c', p', d', i')
  where
    infected = S.member p c
    c' = (if infected then S.delete else S.insert) p c
    p' = add p d'
    d' = (if infected then right else left) d
    i' = if infected then i else i + 1

data Node = Clean | Weakened | Infected | Flagged deriving (Show, Eq)

move2 (!c, !p, !d, !i) = (c', p', d', i')
  where
    s = case M.lookup p c of
        Just a -> a
        _ -> Clean
    next Clean = Weakened
    next Weakened = Infected
    next Infected = Flagged
    next Flagged = Clean
    s' = next s
    c' = if s' == Clean then M.delete p c else M.insert p s' c
    p' = add p d'
    d' = turn d
    i' = if s' == Infected then i + 1 else i
    turn = case s of
        Clean -> left
        Weakened -> id
        Infected -> right
        Flagged -> right . right

part1 (c,w,h) =
    let p = (w`div`2, h`div`2)
        (_,_,_,i) = iterate move (c, p, (0,-1), 0) !! 10000
    in i

part2 (c,w,h) =
    let p = (w`div`2, h`div`2)
        c2 = M.fromList $ map (\x -> (x,Infected)) (S.toList c)
        f a i = if i == 0 then a else f (move2 a) (i - 1)
        (_,_,_,i) = f (c2, p, (0,-1), 0) 10000000
    in i

main = do
    cluster <- parseFromFile parser "input/d22.txt"
    print $ part1 <$> cluster
    print $ part2 <$> cluster
