{-# LANGUAGE BangPatterns #-}

import Debug.Trace

import Text.Parsec
import Text.Parsec.String
import Data.Maybe

import qualified Data.Sequence as Sequence
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V

type Program = V.Vector Instr

data ProgState = ProgState
    { program :: Program
    , programCounter :: Int
    , registers :: Map.Map Char Int
    } deriving Show

type Reg = Char

data Val = Register Reg | Value Int deriving Show

data Instr = Set Reg Val | Sub Reg Val | Mul Reg Val | Jnz Val Val | Add Reg Val | Jgz Val Val | Mod Reg Val deriving Show

value :: Parser Val
value = val <|> reg
  where
    val      = Value <$> (negative <|> positive)
    reg      = Register <$> letter
    negative = negate <$> (char '-' *> positive)
    positive :: Parser Int
    positive = read <$> many1 digit

instruction :: Parser Instr
instruction = set <|> sub <|> mul <|> jnz <|> add <|> jgz <|> mod
  where
    set      = ins2 "set" register value Set
    sub      = ins2 "sub" register value Sub
    mul      = ins2 "mul" register value Mul
    jnz      = ins2 "jnz" value value Jnz

    -- These 3 are not in documentation ;)
    add      = ins2 "add" register value Add
    jgz      = ins2 "jgz" value value Jgz
    mod      = ins2 "mod" register value Mod
    register = letter
    ins2 :: String -> Parser a -> Parser b -> (a -> b -> Instr) -> Parser Instr
    ins2 s p1 p2 c = try
        ( do
            a <- string s *> spaces *> p1
            b <- spaces *> p2
            return $ c a b
        )

instructions :: Parser [Instr]
instructions = many (instruction <* optional spaces)

valueof :: ProgState -> Val -> Int
valueof _ (Value    !x) = x
valueof s (Register !r) = fromMaybe 0 $ Map.lookup r (registers s)

eval :: ProgState -> (ProgState, Bool)
eval state@(ProgState !prog !pc !_)
    | pc >= V.length prog
    = (state, False)
    | otherwise
    = let
          state' = case prog V.! pc of
              Jnz x y ->
                  jump (if valueof state x /= 0 then valueof state y else 1) state
              Jgz x y ->
                  jump (if valueof state x > 0 then valueof state y else 1) state
              i -> jump 1 (eval1 i state)
      in
          (state', True)
  where
    eval1 (Set x y) s = updateReg x y (\_ b -> b) s
    eval1 (Sub x y) s = updateReg x y (-) s
    eval1 (Mul x y) s = updateReg x y (*) s
    eval1 (Add x y) s = updateReg x y (+) s
    eval1 (Mod x y) s = updateReg x y mod s
    eval1 _         _ = undefined
    updateReg x y f s = s
        { registers = Map.insert x
                                 (valueof s (Register x) `f` valueof s y)
                                 (registers s)
        }

jump :: Int -> ProgState -> ProgState
jump i s = s { programCounter = programCounter s + i }

part1 :: Int -> ProgState -> Int
part1 !n !s = case eval s of
    (s', True ) -> part1 n' s'
    (_ , False) -> n
  where
    n' | ismul (program s V.! programCounter s) = n + 1
       | otherwise                              = n
    ismul :: Instr -> Bool
    ismul (Mul _ _) = True
    ismul _         = False

part2 :: ProgState -> Map.Map Char Int
part2 state = part2' state { registers = Map.singleton 'a' 1 }
  where
    part2' !s = case eval s of
        (s', True ) -> part2' s'
        (s', False) -> registers s'

main :: IO ()
main = do
    instructionList <- parseFromFile instructions "input/d23.txt"
    let xs    = V.fromList <$> instructionList
    let state = (\p -> ProgState p 0 Map.empty) <$> xs
    print $ part1 0 <$> state
    optimized <- parseFromFile instructions "input/d23-optimized.txt"
    let ys = V.fromList <$> optimized
    let state2 = (\p -> ProgState p 0 Map.empty) <$> ys
    print $ part2 <$> state2
