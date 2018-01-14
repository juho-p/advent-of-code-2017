import qualified Data.Vector as V

import Data.List
import Data.Char
import Data.Maybe
import qualified Data.Set as S

type Puzzle = V.Vector (V.Vector Char)

readPuzzle :: String -> Puzzle
readPuzzle = V.fromList . map V.fromList . lines

at :: (Int, Int) -> Puzzle -> Char
at (x, y) p = fromMaybe ' ' (p V.!? y >>= (V.!?x))

start :: Puzzle -> (Int, Int)
start puzzle = (fromMaybe 0 $ V.elemIndex '|' (puzzle V.! 0), 0)

next :: Puzzle -> S.Set (Int, Int) -> (Int, Int) -> [(Int, Int)]
next puzzle seen curr@(cx, cy) = case find valid directions of
    Just d  -> [move d]
    Nothing -> maybe [] jumpMoves (find valid jumps)
  where
    move (dx, dy) = (cx+dx, cy+dy)
    jumpMoves p@(dx,dy) = [move (dx `div` 2, dy `div` 2), move p]
    directions = [(1, 0), (0, -1), (-1, 0), (0, 1)]
    jumps      = map (\(x, y) -> (x + x, y + y)) directions
    valid (dx, dy) | S.member n seen = False
                   | dx == 0 && (ch == '-' || ch0 == '-') = False
                   | dy == 0 && (ch == '|' || ch0 == '|') = False
                   | dx == 0 && ch == '|' = True
                   | dy == 0 && ch == '-' = True
                   | ch == '+' = True
                   | isLetter ch = True
                   | otherwise = False
      where
        n@(nx, ny) = move (dx, dy)
        ch0 = at curr puzzle
        ch = at (nx, ny) puzzle

path :: Puzzle -> [(Int,Int)]
path puzzle = path' S.empty (start puzzle)
  where
    path' seen curr =
        case next puzzle seen curr of
            [p] -> p : rest p
            [j,p] -> j : p : rest p
            _ -> []
      where rest = path' (S.insert curr seen)
            

main = do
    rows <- readFile "input/d19.txt"
    let puzzle = readPuzzle rows
    let xs = path puzzle
    let chars = map (`at` puzzle) xs
    print $ filter isLetter chars
    print $ 1 + length chars
