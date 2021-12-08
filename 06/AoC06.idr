import System
import System.File.Virtual
import System.File.ReadWrite

import Data.List1
import Data.Either
import Data.String

%default total

-- https://github.com/idris-lang/Idris2/pull/2163/commits/49035a8066e77b17cd3601724430bc98086ef6bc
die : HasIO io => String -> io a
die str = do putStrLn str; exitFailure

||| Perform a laternfish step:
|||   If the timer has reached 0, create two more
|||   timers:
|||     1) A timer set to 6, representing the existing lanternfish.
|||     2) A timer set to 8, representing the new lanternfish.
|||   Else, if the timer hasn't reached 0, decrement it.
step : Nat -> Either Nat (List Nat)
step 0 = Right [6, 8]
step (S k) = Left k

||| Perform `step` on each of the values in a list, returning the resulting
||| list.
stepList : List Nat -> List Nat
stepList xs =
  let stepped = map step xs
      ls = lefts stepped
      rs = rights stepped
  in ls ++ foldr (++) [] rs


------------
-- PART 1 --
------------

||| Simulate and return the number of fish after `nSteps` days
solvePart1 : (fish : List Nat) -> (nSteps : Nat) -> Nat
solvePart1 fish 0 = length fish
solvePart1 fish (S k) = solvePart1 (stepList fish) k

------------------
-- PARSING+MAIN --
------------------

||| Parse the line representing the initial state of lanternfish
parseInit : String -> Maybe (List Nat)
parseInit s =
  do let (head ::: tail) = split (== ',') s
     n <- parsePositive head
     ns <- traverse parsePositive tail
     pure (n :: ns)

main : IO ()
main =
  do (Right l) <- fGetLine stdin
        | Left err => die $ show err
     (Just init) <- pure $ parseInit l
        | Nothing => die "Failed to parse init."
     putStrLn $ "Part 1: " ++ show (solvePart1 init 80)

