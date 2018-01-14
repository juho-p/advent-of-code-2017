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
    , frequency :: Int
    , queue :: Sequence.Seq Int
    } deriving Show

type Reg = Char

data Val = Register Reg | Value Int deriving Show

data Instr = Snd Val | Set Reg Val | Add Reg Val | Mul Reg Val | Mod Reg Val | Rcv Reg | Jgz Val Val deriving Show

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
    rcv      = ins1 "rcv" register Rcv
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
valueof _ (Value    !x) = x
valueof s (Register !r) = fromMaybe 0 $ Map.lookup r (registers s)

eval :: ProgState -> ProgState
eval state
    | programCounter state >= V.length (program state) = error
        "PC out of bounds"
    | otherwise = case program state V.! programCounter state of
        Rcv r | valueof state (Register r) == 0 -> next state
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

jump :: Int -> ProgState -> ProgState
jump i s = s { programCounter = programCounter s + i }

data SendState = Receiving Reg | Sending Int

eval2 :: ProgState -> (SendState, ProgState)
eval2 state@(ProgState !prg !pc !regs _ !q) =
    case prg V.! pc of
      Snd v -> (Sending (valueof state v), jump 1 state)
      Rcv r | Sequence.null q -> (Receiving r, state) 
            | otherwise -> eval2 $ state
                { registers = Map.insert r (Sequence.index q 0) regs
                , programCounter = pc + 1
                , queue = Sequence.drop 1 q
                }
      Jgz x y ->
        eval2 $ jump (if valueof state x > 0 then valueof state y else 1) state
      i -> eval2 (eval1 i state) { programCounter = 1 + pc }
  where
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

run2 :: Program -> Int
run2 program =
    let makeProgram i = ProgState program 0 (Map.singleton 'p' i) 0 Sequence.empty
        p0 = (Sending 0, makeProgram 0)
        p1 = (Sending 0, makeProgram 1)
    in
        run2' 0 p0 p1
  where
    run2' !sends !p0 !p1
        | waiting p0 && waiting p1 = sends
        | waiting p1 = let (_, a,b) = advance 0 p0 p1 in run2' sends a b
        | otherwise = let (sends', a,b) = advance sends p1 p0 in run2' sends' b a
    waiting (Receiving _, s)
        | Sequence.null $ queue s = True
        | otherwise = False
    waiting _ = False
    advance sends (_, state1) b@(send2, state2) = case eval2 state1 of
        a@(Sending i, _) ->
            (sends + 1, a, (send2, state2 { queue = queue state2 Sequence.|> i }))
        a -> (sends, a, b)

main :: IO ()
main = do
    instructionList <- parseFromFile instructions "d18.txt"
    let xs = V.fromList <$> instructionList
    let state = (\p -> ProgState p 0 Map.empty 0 Sequence.empty) <$> xs
    print $ eval <$> state
    print $ run2 <$> xs

