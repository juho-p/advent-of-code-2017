import Data.Int
import Data.Maybe
import Data.Char
import Data.List

import Text.Parsec
import Text.Parsec.String

import qualified Data.Vector.Unboxed as V

type Prog = Int8

type Programs = V.Vector Prog

start :: Programs
start = V.fromList [0 .. 15]

data Move = Spin Int | Exchange Int Int | Partner Prog Prog deriving Show

eval :: Move -> Programs -> Programs
eval (Spin a) xs = V.drop (V.length xs - a) xs V.++ V.take (V.length xs - a) xs
eval (Exchange a b) xs = xs V.// [(a, xs V.! b), (b, xs V.! a)]
eval (Partner a b) xs = V.map f xs
  where
    f p | p == a    = b
        | p == b    = a
        | otherwise = p

evalMoves :: [Move] -> Programs -> Programs
evalMoves []     x  = x
evalMoves (x:xs) ps = evalMoves xs $ seq ps (eval x ps)

parser :: Parser Move
parser = spin <|> exchange <|> partner <* optional spaces
  where
    num      = read <$> many1 digit
    spin     = Spin <$> (char 's' *> num)
    exchange = do
        a <- char 'x' *> num
        b <- char '/' *> num
        return $ Exchange a b
    partner = do
        a <- char 'p' *> letter
        b <- char '/' *> letter
        return $ Partner (program a) (program b)
    program c = fromIntegral $ ord c - ord 'a'

showPrograms :: Programs -> String
showPrograms ps = map showProg (V.toList ps)
    where showProg = chr . (+ord 'a') . fromIntegral

solveBillionRuns :: [Move] -> Programs
solveBillionRuns moves = runs !! (n - 1)
  where
    runs = tail $ iterate (seq moves $ evalMoves moves) start
    loop = fromMaybe undefined $ elemIndex start runs
    n    = 1000000000 `rem` (loop + 1)

main :: IO ()
main = do
    moves <- parseFromFile (parser `sepBy` char ',') "input/d16.txt"
    let part1 = (`evalMoves`start) <$> moves
    print $ showPrograms <$> part1

    print $ showPrograms <$> (solveBillionRuns <$> moves)
