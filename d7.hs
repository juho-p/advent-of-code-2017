import Data.List

import qualified Data.Map as Map
import qualified Data.Set as Set

import Text.Parsec
import Text.Parsec.String

type Name = String
type Weight = Int

data Program = Program Name Weight [Name] deriving Show

integer :: Parser Int
integer = read <$> many1 digit

nameList :: Parser [Name]
nameList = sepBy1 (many1 letter) (string ", ")

program :: Parser Program
program = do
    name   <- many1 letter
    weight <- string " (" *> integer <* char ')'
    names  <- string " -> " *> nameList <|> return []
    _      <- optional $ char '\n'
    return $ Program name weight names

programs :: Parser [Program]
programs = many program

part1 :: [Program] -> Maybe Program
part1 progs = find (not . isAnyChild) progs
  where
    above (Program _ _ xs) = xs
    children = Set.fromList $ concatMap above progs
    isAnyChild (Program name _ _) = Set.member name children

name :: Program -> Name
name (Program s _ _) = s

programMap :: [Program] -> Map.Map Name Program
programMap = Map.fromList . map (\p -> (name p, p))

part2 :: [Program] -> Maybe [(Program, Int)]
part2 progs = fmap result (find match progs)
  where
    byName    = programMap progs
    fromNames = map (byName Map.!)
    children (Program _ _ ss) = fromNames ss
    stackWeight (Program _ w ss) = sum (w : (map stackWeight $ fromNames ss))
    childWeights p = map stackWeight $ children p
    match p = Set.size (Set.fromList $ childWeights p) > 1
    result p = children p `zip` childWeights p

main :: IO ()
main = do
    progs <- parseFromFile programs "input/d7.txt"
    print $ part1 <$> progs
    print $ part2 <$> progs
