import Data.List

cA :: Integer
cA = 16807
cB :: Integer
cB = 48271

next :: Integer -> Integer -> Integer
next c x = x * c `rem` 2147483647

match :: Integer -> Integer -> Bool
match a b = am == bm
    where am = a `mod` d
          bm = b `mod` d
          d = 2^16

gen :: Integer -> Integer -> [Integer]
gen c x = tail $ iterate (next c) x

part1 :: Integer -> Integer -> Integer
part1 sa sb =
    let
        as = gen cA sa
        bs = gen cB sb
        matches = take 40000000 $ zipWith match as bs
    in foldl' (\acc b -> if b then acc+1 else acc) 0 matches

part2 :: Integer -> Integer -> Integer
part2 sa sb =
    let
        divBy d x = x `mod` d == 0
        as = filter (divBy 4) $ gen cA sa
        bs = filter (divBy 8) $ gen cB sb
        matches = take 5000000 $ zipWith match as bs
    in foldl' (\acc b -> if b then acc+1 else acc) 0 matches

main = do
    let a = 591
    let b = 393
    --print $ part1 a b
    print $ part2 a b

