import Data.Char

solve :: ([Int] -> [Int]) -> String -> Int
solve f s = sum [x | (x,y) <- zip xs ys, x == y]
    where
        xs = map digitToInt . filter isDigit $ s
        ys = f xs

part1 xs = last xs : xs

part2 xs = (drop ((length xs) `div` 2) xs) ++ xs

main = do
    input <- readFile "d1.txt"
    print $ solve part1 input
    print $ solve part2 input
