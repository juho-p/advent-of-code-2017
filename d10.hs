import qualified Data.Vector.Unboxed as V
import Data.Char
import Data.List
import Data.Function
import Data.Bits
import Numeric

type Numbers = V.Vector Int
type Length = Int
type Index = Int

reverseSliceWithIndex :: Index -> Length -> Numbers -> [(Index, Int)]
reverseSliceWithIndex i len v =
    if i + len <= V.length v then
        zip [i..] (reverseList (V.slice i len v))
    else
        let a = V.slice i (V.length v - i) v
            b = V.slice 0 (len - V.length v + i) v
        in zip ([i..(V.length v - 1)] ++ [0..]) (reverseList b ++ reverseList a)

        where reverseList = V.toList . V.reverse

reverseInside :: Index -> Length -> Numbers -> Numbers
-- using unsafeUpd, but what could go wrong??
reverseInside i k v = v `V.unsafeUpd` reverseSliceWithIndex i k v

tieKnots :: Index -> Index -> [Length] -> Numbers -> Numbers
tieKnots _ _ [] xs = xs
tieKnots skipsize position (len:lengths) numbers =
    let nextPosition = (position + len + skipsize) `mod` V.length numbers
        nextNumbers = reverseInside position len numbers
    in tieKnots (skipsize + 1) nextPosition lengths nextNumbers

part1 :: [Length] -> Int
part1 lengths = a * b
    where (a:b:_) = V.toList $ tieKnots 0 0 lengths (V.fromList [0..255])

densify :: [Int] -> Int
densify = foldl' xor 0

part2 :: [Length] -> String
part2 lengths =
    let
        paddedLengths = lengths ++ [17, 31, 73, 47, 23]
        repeatedLengths = concat $ replicate 64 paddedLengths
        sparseHash = V.toList $ tieKnots 0 0 repeatedLengths (V.fromList [0..255])
        groups = groupBy ((==) `on` (\(i,_) -> i `div` 16)) (zip [0..] sparseHash)
        denseHash = map (densify . map snd) groups
    in concatMap (`showHex` "") denseHash

parsePart1 :: String -> [Length]
parsePart1 = map read . words . map (\c -> if c == ',' then ' ' else c)

parsePart2 :: String -> [Length]
parsePart2 = map ord . filter (not . isSpace)

main :: IO ()
main = do
    lengths <- parsePart1 <$> readFile "input/d10.txt"
    print $ part1 lengths

    print $ part2 []
    print $ part2 (map ord "AoC 2017")

    lengths2 <- parsePart2 <$> readFile "input/d10.txt"
    print $ part2 lengths2
