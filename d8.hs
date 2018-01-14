import Text.Parsec
import Text.Parsec.String
import qualified Data.Map as Map
import Data.Maybe

data Command = Inc | Dec

readCmd :: String -> Command
readCmd "inc" = Inc
readCmd "dec" = Dec
readCmd _ = error "wrong cmd" -- should do something better

type Register = String

type Comparison = Int -> Int -> Bool

data Predicate = Predicate Comparison Register Int

readCond :: String -> Comparison
readCond "<"  = (<)
readCond ">"  = (>)
readCond "==" = (==)
readCond "!=" = (/=)
readCond "<=" = (<=)
readCond ">=" = (>=)
readCond s    = error $ "op = " ++ s -- this is just bad...

data Instruction = Instr Register Command Int Predicate

register :: Parser Register
register = many1 letter

command :: Parser Command
command = readCmd <$> many1 letter

integer :: Parser Int
integer = positive <|> negative
  where
    positive = read <$> many1 digit
    negative = negate <$> (char '-' *> positive)

predicate :: Parser Predicate
predicate = do
    r <- register
    _ <- char ' '
    p <- readCond <$> many1 (noneOf " ")
    _ <- char ' '
    x <- integer
    return $ Predicate p r x

instruction :: Parser Instruction
instruction = do
    reg <- register
    _ <- char ' '
    cmd <- command
    _ <- char ' '
    amt <- integer
    _ <- string " if "
    p <- predicate
    optional $ char '\n'

    return $ Instr reg cmd amt p

program :: Parser [Instruction]
program = many instruction

type Registers = Map.Map Register Int

eval :: Instruction -> Registers -> Registers
eval (Instr reg cmd x p) state = if matches p
    then Map.insert reg (run cmd (regval reg)) state
    else state
  where
    run :: Command -> Int -> Int
    run Inc = (x+)
    run Dec = \a -> a - x
    regval r = fromMaybe 0 $ Map.lookup r state
    matches (Predicate p' r a) = p' (regval r) a

evalProgram :: [Instruction] -> Registers -> [Registers]
evalProgram []     s = [s]
evalProgram (x:xs) s = s : evalProgram xs (eval x s)

maxValue :: Registers -> Int
maxValue xs = if null xs then 0 else maximum $ Map.elems xs -- remember to check nulls, t. javacoder

result :: [Instruction] -> (Int, Int)
result xs = (maximum values, last values)
  where
    results = evalProgram xs Map.empty
    values  = map maxValue results

trySolve :: String -> Either ParseError (Int, Int)
trySolve input =
    let prog = parse program "prog" input
    in  result <$> prog

main :: IO ()
main = do
    input <- readFile "input/d8.txt"
    print $ trySolve input
    return ()
