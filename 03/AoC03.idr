import System
import System.File.Virtual
import System.File.ReadWrite

import Data.Vect
import Data.List
import Data.String
import Data.Binary
import Data.Binary.Digit

-- https://github.com/idris-lang/Idris2/pull/2163/commits/49035a8066e77b17cd3601724430bc98086ef6bc
die : HasIO io => String -> io a
die str = do putStrLn str; exitFailure

||| Convert a digit as a character to the corresponding `Digit`
parseDigit : Char -> Maybe Digit
parseDigit '0' = Just O
parseDigit '1' = Just I
parseDigit _   = Nothing

Entry : Type
Entry = Vect 12 Digit

||| Convert a string to the corresponding binary number entry
parseEntry : String -> Maybe Entry
parseEntry s =
  case unpack s of
       [c0, c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11] =>
          do d0 <- parseDigit c0
             d1 <- parseDigit c1
             d2 <- parseDigit c2
             d3 <- parseDigit c3
             d4 <- parseDigit c4
             d5 <- parseDigit c5
             d6 <- parseDigit c6
             d7 <- parseDigit c7
             d8 <- parseDigit c8
             d9 <- parseDigit c9
             d10 <- parseDigit c10
             d11 <- parseDigit c11
             Just [d0, d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11]
       _ => Nothing

||| Find the most common bit at the given position
mostCommonBitAt : List Entry -> (pos : Nat) -> {auto 0 ok : pos `LT` 12} -> Digit
mostCommonBitAt es pos =
  let bits     = map (index (natToFinLT pos)) es
      (is, os) = partition isI bits
  in if length is > length os
        then I
        else O

||| Flip the bits in a binary number
invert : Bin -> Bin
invert [] = []
invert (O :: bs) = I :: invert bs
invert (I :: bs) = O :: invert bs

||| Calculate the gamma and epsilon values as per the puzzle
calcGammaEpsilon : List Entry -> (Nat, Nat)
calcGammaEpsilon es =
  -- would be nice to use `map` or something, but proving All is tedious
  let b0  = mostCommonBitAt es 0
      b1  = mostCommonBitAt es 1
      b2  = mostCommonBitAt es 2
      b3  = mostCommonBitAt es 3
      b4  = mostCommonBitAt es 4
      b5  = mostCommonBitAt es 5
      b6  = mostCommonBitAt es 6
      b7  = mostCommonBitAt es 7
      b8  = mostCommonBitAt es 8
      b9  = mostCommonBitAt es 9
      b10 = mostCommonBitAt es 10
      b11 = mostCommonBitAt es 11
      -- `reverse` is needed since Bin is rigth-to-left ordered
      bGamma = reverse [b0, b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11]
      bEpsilon = invert bGamma
  in (toNat bGamma, toNat bEpsilon)

||| Calculate the power of the submarine: multiply the values together
calcPower : (Nat, Nat) -> Nat
calcPower (gamma, epsilon) = gamma * epsilon

solvePart1 : (input : List String) -> Maybe Nat
solvePart1 input =
  do es <- traverse parseEntry input
     let gE = calcGammaEpsilon es
     pure $ calcPower gE

main : IO ()
main =
  do (Right input) <- fRead stdin
       | Left err => die $ show err
     (Just res) <- pure $ solvePart1 (lines input)
       | Nothing => die "Error in input."
     putStrLn $ "Part 1: " ++ show res

