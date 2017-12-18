import Text.Parsec
import Text.Parsec.String
import Data.Maybe

import qualified Data.Map as Map
import qualified Data.Vector as V

type Program = V.Vector Instr

data ProgState = ProgState
    { program :: Program
    , programCounter :: Int
    , registers :: Map.Map Char Int
    , frequency :: Int
    } deriving Show

type Reg = Char

data Val = Register Reg | Value Int deriving Show

data Instr = Snd Val | Set Reg Val | Add Reg Val | Mul Reg Val | Mod Reg Val | Rcv Val | Jgz Val Val deriving Show

value :: Parser Val
value = val <|> reg
  where
    val      = Value <$> (negative <|> positive)
    reg      = Register <$> letter
    negative = negate <$> (char '-' *> positive)
    positive :: Parser Int
    positive = read <$> many1 digit

instruction :: Parser Instr
instruction = snd <|> set <|> add <|> mul <|> mod <|> rcv <|> jgz
  where
    snd      = ins1 "snd" value Snd
    set      = ins2 "set" register value Set
    add      = ins2 "add" register value Add
    mul      = ins2 "mul" register value Mul
    mod      = ins2 "mod" register value Mod
    rcv      = ins1 "rcv" value Rcv
    jgz      = ins2 "jgz" value value Jgz
    register = letter
    ins1 :: String -> Parser a -> (a -> Instr) -> Parser Instr
    ins1 s p c = try (string s *> spaces *> (c <$> p))
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
valueof _ (Value    x) = x
valueof s (Register r) = fromMaybe 0 $ Map.lookup r (registers s)

eval :: ProgState -> ProgState
eval state
    | programCounter state >= V.length (program state) = error
        "PC out of bounds"
    | otherwise = case program state V.! programCounter state of
        Rcv v | valueof state v == 0 -> next state
              | otherwise            -> state
        Jgz x y
            | valueof state x > 0 -> eval
                (state { programCounter = programCounter state + valueof state y })
            | otherwise -> next state
        i -> next (eval1 i state)
  where
    next s = eval s { programCounter = 1 + programCounter s }
    eval1 (Snd v  ) s = s { frequency = valueof s v }
    eval1 (Set x y) s = updateReg x y (\_ b -> b) s
    eval1 (Add x y) s = updateReg x y (+) s
    eval1 (Mul x y) s = updateReg x y (*) s
    eval1 (Mod x y) s = updateReg x y mod s
    eval1 _         s = s
    updateReg x y f s = s
        { registers = Map.insert x
                                 (valueof s (Register x) `f` valueof s y)
                                 (registers s)
        }

main :: IO ()
main = do
    xs <- parseFromFile instructions "d18.txt"
    let state = (\p -> ProgState (V.fromList p) 0 Map.empty 0) <$> xs
    print $ eval <$> state

