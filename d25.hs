{-# LANGUAGE BangPatterns #-}

import Text.Parsec
import Text.Parsec.String
import Data.List

import qualified Data.Map as M
import qualified Data.Set as S

type Value = Bool
type Name = Char
data Direction = MoveLeft | MoveRight deriving (Show, Eq)

type Instruction = (Value, Value, Direction, Name)

type Program = (Name, Int, M.Map Name [Instruction])

type MachineState = (Name, Int, S.Set Int)

state :: Parser (Name, [Instruction])
state = do
    name         <- string "In state " *> letter <* string ":" <* spaces
    instructions <- many1 instruction
    return (name, instructions)
  where
    value = (=='1')
    move "left"  = MoveLeft
    move "right" = MoveRight
    move _       = undefined
    instruction = do
        condition <-
            try (string "If the current value is ") *> digit <* string ":" <* spaces
        write <- string "- Write the value " *> digit <* string "." <* spaces
        dir   <-
            string "- Move one slot to the "
            *> many (noneOf ".")
            <* string "."
            <* spaces
        next <-
            string "- Continue with state " *> letter <* string "." <* spaces

        return (value condition, value write, move dir, next)

program :: Parser Program
program = do
    start  <- string "Begin in state " *> letter <* string "." <* spaces
    cycles <-
        string "Perform a diagnostic checksum after "
        *> many1 digit
        <* string " steps."
        <* spaces
    states <- many1 state
    return (start, read cycles, M.fromList states)

exec :: M.Map Name [Instruction] -> MachineState -> MachineState
exec prog state@(!s, !i, !values) =
    maybe state nextState (find (match value) instructions)
  where
    instructions = prog M.! s
    value = S.member i values
    match a (x, _,_,_) = a == x
    nextState (_, write, move, cont) =
        ( cont
        , case move of
            MoveLeft -> i - 1
            MoveRight -> i + 1
        , if write
            then S.insert i values
            else S.delete i values
        )

solve (start, n, prog) = S.size values
  where
    (_, _, values) = solve' (start, 0, S.empty) n
    solve' x 0  = x
    solve' !x !n' = solve' (exec prog x) (n' - 1)

main :: IO ()
main = do
    p <- parseFromFile program "input/d25.txt"
    print (solve <$> p)
