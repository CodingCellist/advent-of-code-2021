import System
import System.File.Virtual
import System.File.ReadWrite

import Data.String

-- https://github.com/idris-lang/Idris2/pull/2163/commits/49035a8066e77b17cd3601724430bc98086ef6bc
die : HasIO io => String -> io a
die str = do putStrLn str; exitFailure

loop : HasIO io => (n1, count : Nat) -> io Nat
loop n1 count =
  do Right l <- fGetLine stdin
       | Left err => die $ show err
     (Just n2) <- pure $ parsePositive l
       | Nothing => pure count
     loop n2 $ if n2 > n1 then count + 1 else count

main : IO ()
main =
  do Right l <- fGetLine stdin
       | Left err => die $ show err
     (Just n1) <- pure $ parsePositive l
       | Nothing => die "No initial integer."
     res <- loop n1 0
     putStrLn $ "#increases: " ++ show res

