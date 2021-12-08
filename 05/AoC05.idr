import System
import System.File.Virtual
import System.File.ReadWrite

import Data.Vect
import Data.List1

%default total

-- https://github.com/idris-lang/Idris2/pull/2163/commits/49035a8066e77b17cd3601724430bc98086ef6bc
die : HasIO io => String -> io a
die str = do putStrLn str; exitFailure

Coord : Type
Coord = (Nat, Nat)

Line : Type
Line = (Coord, Coord)

-- we don't have this??
pairMax : Ord ty => (ty, ty) -> ty
pairMax (x, y) = max x y

-- or this??
listMax : Ord ty => List1 ty -> ty
listMax (head ::: tail) = listMax' head tail
  where
    listMax' : Ord t => (currMax : t) -> List t -> t
    listMax' currMax [] = currMax
    listMax' currMax (x :: xs) =
      if currMax < x
         then listMax' x xs
         else listMax' currMax xs

||| Find the size of the board, i.e. the greatest end value
boardSize : List1 Line -> Nat
boardSize xs = listMax $ map (\(c1, c2) => max (pairMax c1) (pairMax c2)) xs

||| Create the board for plotting the lines on
createBoard : (lines : List1 Line) -> Vect (length lines) (Vect (boardSize lines) Nat)
createBoard lines = replicate (length lines) (replicate (boardSize lines) 0)

plotLines :  (lines : List1 Line)
--          -> (board : Vect (length lines) (Vect (boardSize lines) Nat)
          -> Vect (length lines) (Vect (boardSize lines) Nat)
plotLines lines@(head ::: []) =
  let board = createBoard lines
  in ?plotLines_rhs_1
plotLines lines@(head ::: ls@(_ :: _)) = ?plotLines_rhs_2

