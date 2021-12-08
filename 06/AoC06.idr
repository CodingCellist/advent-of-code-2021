import System
import System.File.Virtual
import System.File.ReadWrite
import System.Clock

import Data.Vect
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


||| Simulate and return the number of fish after `nSteps` days
simulate : (fish : List Nat) -> (nSteps : Nat) -> Nat
simulate fish 0 = length fish
simulate fish (S k) = simulate (stepList fish) k

------------
-- PART 1 --
------------

solvePart1 : (init : List Nat) -> Nat
solvePart1 init = simulate init 80

------------
-- PART 2 --
------------

FishTimers : Type
FishTimers = Vect 9 Nat

blank : FishTimers
blank = replicate 9 0

nFish : FishTimers -> Nat

||| Update the timers by creating as many new `eights` as there were `zeroes`
||| (the lanternfish spawning new fish), and updating the `sixes` to the
||| previous `sevens` (the young laternfish which just reached maturity) plus
||| the `zeroes` (the old laternfish which have just reset).
step' : FishTimers -> FishTimers
step' [zeroes, ones, twos, threes, fours, fives, sixes, sevens, eights] =
   [ones, twos, threes, fours, fives, sixes, sevens + zeroes, eights, zeroes]

||| Simulate and return the number of fish after `nSteps` days, but using a much
||| more efficient storage method.
simulate' : FishTimers -> (nSteps : Nat) -> FishTimers
simulate' ts 0 = ts
simulate' ts (S k) = simulate' (step' ts) k

solvePart2 : (init : FishTimers) -> Nat
solvePart2 init = sum $ simulate' init 256

solvePart1' : (init : FishTimers) -> Nat
solvePart1' init = sum $ simulate' init 80


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

parseDigit : String -> FishTimers -> Maybe FishTimers
parseDigit s ts =
   do n <- parsePositive s
      case n of
           Z => Just $ updateAt (natToFinLT 0) ((+) 1) ts
           1 => Just $ updateAt (natToFinLT 1) ((+) 1) ts
           2 => Just $ updateAt (natToFinLT 2) ((+) 1) ts
           3 => Just $ updateAt (natToFinLT 3) ((+) 1) ts
           4 => Just $ updateAt (natToFinLT 4) ((+) 1) ts
           5 => Just $ updateAt (natToFinLT 5) ((+) 1) ts
           6 => Just $ updateAt (natToFinLT 6) ((+) 1) ts
           7 => Just $ updateAt (natToFinLT 7) ((+) 1) ts
           8 => Just $ updateAt (natToFinLT 8) ((+) 1) ts
           _ => Nothing

vectAdd : Num ty => Vect n ty -> Vect n ty -> Vect n ty
vectAdd [] [] = []
vectAdd (x :: xs) (y :: ys) = (x + y) :: vectAdd xs ys

parseInit' : String -> Maybe FishTimers
parseInit' s =
  do let (head ::: tail) = split (== ',') s
     n <- parseDigit head blank
     ns <- traverse (flip parseDigit $ blank) tail
     pure $ foldr vectAdd n ns

main : IO ()
main =
  do (Right l) <- fGetLine stdin
        | Left err => die $ show err
     (Just init) <- pure $ parseInit l
        | Nothing => die "Failed to parse init."
     (Just init2) <- pure $ parseInit' l
        | Nothing => die "Failed to parse' init."
     t1 <- clockTime Monotonic
     let p1 = solvePart1 init
     t2 <- clockTime Monotonic
     t3 <- clockTime Monotonic
     let p2 = solvePart1' init2
     t4 <- clockTime Monotonic

     putStrLn $ "Part 1: " ++ show p1 ++ " (results match: " ++ show (p1 == p2) ++ ")"

     putStrLn $ "Part 2: " ++ show (solvePart2 init2)

     putStrLn $ "Naïve solution " ++ show (timeDifference t2 t1)
     putStrLn $ "Vector solution " ++ show (timeDifference t4 t3)

