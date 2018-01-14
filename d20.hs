import Text.Parsec
import Text.Parsec.String

import Data.List
import qualified Data.Set as Set

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

part1 xs = head xs'
    where
        score (Particle p v a) = (len a, len v, len p)
        xs' = sortOn (score . snd) (zip [0..] xs)
        len (x,y,z) = abs x + abs y + abs z

part2 xs = map length $ iterate updateAll xs
    where
        update (Particle p v a) =
            let
                v' = v `add3` a
                p' = p `add3` v'
            in Particle p' v' a
        add3 (a,b,c) (d,e,f) = (a+d, b+e, c+f)
        updateAll xs' =
            let ys = update <$> xs'
                ks = kills ys
            in filter (not . (`Set.member` ks) . position) ys
        kills xs' = snd $ foldl' (\(ps, ks) (Particle p _ _) ->
            if Set.member p ps then (ps, Set.insert p ks)
            else (Set.insert p ps, ks)) (Set.empty, Set.empty) xs'

main :: IO ()
main = do
    xs <- parseFromFile particles "input/d20.txt"
    print (part1 <$> xs)
    print (part2 <$> xs)
