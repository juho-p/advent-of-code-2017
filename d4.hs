import Data.List

hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates [] = False
hasDuplicates (x:xs) = x `elem` xs || hasDuplicates xs

validPassword :: String -> Bool
validPassword s = not (hasDuplicates (words s))

validPasswordDeluxe :: String -> Bool
validPasswordDeluxe s = not (hasDuplicates (map sort (words s)))

validPasswordCount :: (String -> Bool) -> [String] -> Int
validPasswordCount isValid = length . filter isValid

part1 :: [String] -> Int
part1 = validPasswordCount validPassword


part2 :: [String] -> Int
part2 = validPasswordCount validPasswordDeluxe

main :: IO ()
main = do
    input <- readFile "d4.txt"
    let passwords = lines input
    print $ part1 passwords
    print $ part2 passwords
