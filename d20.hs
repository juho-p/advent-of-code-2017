import Text.Parsec
import Text.Parsec.String

import Data.List

type Vec3 = (Int, Int, Int)

data Particle = Particle
    { position :: Vec3
    , velocity :: Vec3
    , acceleration :: Vec3
    } deriving Show

particle :: Parser Particle
particle = do
    p <- string "p=" *> vec3 <* sep
    v <- string "v=" *> vec3 <* sep
    a <- string "a=" *> vec3 <* newline
    return $ Particle p v a
      where
        sep :: Parser ()
        sep = optional spaces <* char ',' <* optional spaces
        vec3 = do
            x <- char '<' *> optional spaces *> int
            y <- sep *> int
            z <- sep *> int <* optional spaces <* char '>'
            return (x, y, z)
        int = negInt <|> posInt
        negInt = char '-' *> (negate . read <$> many1 digit)
        posInt = read <$> many1 digit

particles :: Parser [Particle]
particles = many particle

len (x,y,z) = abs x + abs y + abs z

part1 xs = head xs'
    where score (Particle p v a) = (len a, len v, len p)
          xs' = sortOn (score . snd) (zip [0..] xs)

main :: IO ()
main = do
    xs <- parseFromFile particles "d20.txt"
    print (part1 <$> xs)
