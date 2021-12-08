import System
import System.File.Virtual
import System.File.ReadWrite

import Data.Nat
import Data.Vect
import Data.List
import Data.List1
import Data.String
import Data.Either
import Decidable.Equality

%default total

-- https://github.com/idris-lang/Idris2/pull/2163/commits/49035a8066e77b17cd3601724430bc98086ef6bc
die : HasIO io => String -> io a
die str = do putStrLn str; exitFailure

BoardSize : Nat
BoardSize = 5

BingoBoard : Type
BingoBoard = Vect BoardSize (Vect BoardSize (Nat, Bool))
--           ^ rows          ^ columns
{- r/c   0 1 2 3 4
 - 0  [[ a b c d e ]
 - 1   [ f g h i j ]
 - 2   [ k l m n o ]
 - 3   [ p q r s t ]
 - 4   [ u v x y z ]]
 -}

||| Check whether the row is complete
checkRow :  (board : BingoBoard)
         -> (r : Nat) -> {auto 0 ok : LT r BoardSize}
         -> Bool
checkRow board r =
   let row = index (natToFinLT r) board
   in all id $ map snd row

||| Check whether the col is complete
checkCol :  (board : BingoBoard)
         -> (c : Nat) -> {auto 0 ok : LT c BoardSize}
         -> Bool
checkCol board c = checkRow (transpose board) c

||| Check whether a board has won
hasWon : BingoBoard -> Bool
hasWon xs =
   hasWon' xs 4
   where
      hasWon' : BingoBoard -> (n : Nat) -> {auto 0 ok : LT n BoardSize} -> Bool
      hasWon' xs 0 = checkRow xs 0 || checkCol xs 0
      hasWon' xs n@(S k) =
         checkRow xs n || checkCol xs n || hasWon' xs k {ok=lteSuccLeft ok}

||| Call a number on the given bingo board, potentially returning a new one if
||| the number caused an update
call : BingoBoard -> (n : Nat) -> Maybe BingoBoard
call board n =
   do rowIdx <- findIndex (any (== (n, False))) board
      let theRow = index rowIdx board
      newRow <- callRow theRow n
      pure $ replaceAt rowIdx newRow board
   where
      callRow : (row : Vect BoardSize (Nat, Bool)) -> (n : Nat) -> Maybe (Vect BoardSize (Nat, Bool))
      callRow row n =
         case findIndex (== (n, False)) row of
              Nothing => Nothing
              (Just idx) => Just $ updateAt idx (\(v, s) => (v, not s)) row

||| Calculate the score of a board
calcScore : (board : BingoBoard) -> (winningNum : Nat) -> Nat
calcScore board winningNum =
   let (_ ** unmarked) = filter (not . snd) $ concat board
       sum = foldr (+) 0 $ map fst unmarked
   in winningNum * sum

||| Try to find a winning board in the given list of boards
findWinningBoard : (boards : List BingoBoard) -> Maybe BingoBoard
findWinningBoard boards =
    case filter hasWon boards of
         []       => Nothing
         (b :: _) => Just b

updateAffectedBoards : (boards : List BingoBoard) -> (n : Nat) -> List BingoBoard
updateAffectedBoards [] n = []
updateAffectedBoards (b :: bs) n =
   case call b n of
        Nothing => b :: updateAffectedBoards bs n
        (Just b') => b' :: updateAffectedBoards bs n


------------
-- PART 1 --
------------

||| Find the score of the first board which would win
solvePart1 : (called : List Nat) -> (boards : List BingoBoard) -> Maybe Nat
solvePart1 [] boards = Nothing
solvePart1 (n :: ns) boards =
   do boards' <- pure $ updateAffectedBoards boards n
      case findWinningBoard boards' of
           Nothing => solvePart1 ns boards'
           (Just b) => Just $ calcScore b n


------------
-- PART 2 --
------------

||| Find the last board which would win
solvePart2 : (called : List Nat) -> (boards : List BingoBoard) -> Maybe Nat
solvePart2 [] _ = Nothing
solvePart2 _ [] = Nothing

-- if we have a single board, check if it's the last to win
solvePart2 (n :: ns) (b :: []) =
  do b' <- call b n
     if hasWon b'
        then Just $ calcScore b' n
        -- keep calling numbers until the board wins or we run out of numbers
        else solvePart2 ns [b']

-- if we have more than one board, update+filter them, and recurse on the result
solvePart2 (n :: ns) boards =
   do -- only keep the boards which haven't won after an update
      boards' <- pure $ filter (not . hasWon) (updateAffectedBoards boards n)
      case boards' of
           (b :: []) =>
                if (hasWon b) then Just (calcScore b n) else solvePart2 ns boards'
           _ => solvePart2 ns boards'


--------------------
-- PARSING + MAIN --
--------------------

||| Parse a string representation of a row
parseRow : String -> Maybe (Vect BoardSize (Nat, Bool))
parseRow s =
   do let ws = assert_total $ words s
      ns <- traverse parsePositive ws
      -- just a fancy way of creating a pair of (n, False) for each n in ns
      let ps = flip (,) False <$> ns
      toVect BoardSize ps

||| Interpret the strings as rows in a bingo board
parseBoard : Vect BoardSize String -> Maybe BingoBoard
parseBoard ss = traverse parseRow ss

||| Parse the line of numbers called
parseCalledNumbers : String -> Maybe (List Nat)
parseCalledNumbers s =
   case split (== ',') s of
        (head ::: tail) =>
            do h <- parsePositive head
               ts <- traverse parsePositive tail
               pure (h :: ts)

||| Read a board of lines
readBoard : IO (Vect BoardSize String)
readBoard =
   do es <- for [1..5] (\_ => fGetLine stdin)
      case partitionEithers es of
           ([], ls) => case toVect BoardSize ls of
                            Nothing => die "readBoard: `toVect` failed."
                            (Just v) => pure v
           _ => die "Got a file error when reading a board."

partial
main : IO ()
main =
  do (Right rawCalls) <- fGetLine stdin
        | Left err => die $ show err
     ignore $ fGetLine stdin
     let boards = reverse !(readBoards [])  -- boards get added to front of list
     (Just calls) <- pure $ parseCalledNumbers rawCalls
        | Nothing => die "main: parseCalledNumbers returned `Nothing`."
     part1Res <- pure $ solvePart1 calls boards
     putStrLn $ "Part 1: " ++ show part1Res
     part2Res <- pure $ solvePart2 calls boards
     putStrLn $ "Part 2: " ++ show part2Res
  where
    partial
    readBoards : (acc : List BingoBoard) -> IO (List BingoBoard)
    readBoards acc =
      if !(fEOF stdin)     -- bang notation, NOT a boolean `not`
         then pure acc
         else do rawBoard <- readBoard
                 (Just board) <- pure $ parseBoard rawBoard
                    | Nothing => die "loop: parseBoard returned `Nothing`."
                 ignore $ fGetLine stdin    -- skip the empty line separator
                 readBoards (board :: acc)

