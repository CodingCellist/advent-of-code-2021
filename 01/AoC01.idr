import System
import System.File.Virtual
import System.File.ReadWrite

import Data.String

-- https://github.com/idris-lang/Idris2/pull/2163/commits/49035a8066e77b17cd3601724430bc98086ef6bc
die : HasIO io => String -> io a
die str = do putStrLn str; exitFailure

loop : HasIO io => (n1, n2, n3, count : Nat) -> io Nat
loop n1 n2 n3 count =
  do Right l <- fGetLine stdin
       | Left err => die $ show err
     (Just n4) <- pure $ parsePositive l
       | Nothing => pure count
     let w1 = sum [n1, n2, n3]
     let w2 = sum [n2, n3, n4]
     loop n2 n3 n4 $ if w2 > w1 then count + 1 else count

main : IO ()
main =
  do Right l1 <- fGetLine stdin
       | Left err => die $ show err
     Right l2 <- fGetLine stdin
       | Left err => die $ show err
     Right l3 <- fGetLine stdin
       | Left err => die $ show err
     (Just n1) <- pure $ parsePositive l1
       | Nothing => die "No initial integer."
     (Just n2) <- pure $ parsePositive l2
       | Nothing => die "No second integer."
     (Just n3) <- pure $ parsePositive l3
       | Nothing => die "No third integer."
     res <- loop n1 n2 n3 0
     putStrLn $ "#increases: " ++ show res
     exitSuccess

