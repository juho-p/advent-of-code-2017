import Text.Parsec
import Text.Parsec.String

data Thing = Garbage String | Group [Thing] deriving Show

ignore :: Parser String
ignore = many (char '!' *> anyChar)

garbageChar :: Parser Char
garbageChar = ignore *> noneOf "!>" <* ignore

garbage :: Parser String
garbage = char '<' *> ignore *> many garbageChar <* char '>'

group :: Parser [Thing]
group = char '{' *> (thing `sepBy` char ',') <* char '}'

thing :: Parser Thing
thing = (Group <$> group) <|> (Garbage <$> garbage)

totalScore :: Int -> Thing -> Int
totalScore level (Group xs) = level + sum (totalScore (level + 1) <$> xs)
totalScore _ (Garbage _) = 0

garbageCount :: Thing -> Int
garbageCount (Garbage s) = length s
garbageCount (Group xs) = sum $ map garbageCount xs

main :: IO ()
main = do
    root <- parseFromFile thing "d9.txt"
    let part1 = totalScore 1 <$> root
    let part2 = garbageCount <$> root
    print part1
    print part2
