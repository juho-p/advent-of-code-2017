readTable :: String -> [[Int]]
readTable s = map (map read . words) $ lines s

rowDifference xs = high - low
    where low = minimum xs
          high = maximum xs

part1 = sum . map rowDifference

rowDivision xs = head [ d |
        a <- xs,
        b <- xs,
        a > b,
        let (d,r) = a `divMod` b,
        r == 0 ]

part2 = sum . map rowDivision

main = do
    input <- readFile "input/d2.txt"
    let table = readTable input
    print $ part1 table
    print $ part2 table
