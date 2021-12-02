import System
import System.File.Virtual
import System.File.ReadWrite

import Data.String

-- https://github.com/idris-lang/Idris2/pull/2163/commits/49035a8066e77b17cd3601724430bc98086ef6bc
die : HasIO io => String -> io a
die str = do putStrLn str; exitFailure

record Submarine where
  constructor MkSub
  depth : Nat
  hPos  : Nat
  aim   : Nat

data Cmd = Up Nat
         | Down Nat
         | Forward Nat

toCmd : String -> Maybe Cmd
toCmd s =
  case words s of
       (s' :: rawN :: []) =>
          do n <- parsePositive rawN
             cmd' <- toCmd' s'
             Just $ cmd' n
       _ => Nothing
  where
    toCmd' : String -> Maybe (Nat -> Cmd)
    toCmd' "up" = Just Up
    toCmd' "down" = Just Down
    toCmd' "forward" = Just Forward
    toCmd' _ = Nothing

step : (sub : Submarine) -> (cmd : Cmd) -> Submarine
step sub (Up n)      = { aim  $= (\a => minus a n)                   } sub
step sub (Down n)    = { aim  $= plus n                              } sub
step sub (Forward n) = { hPos $= plus n, depth $= plus (sub.aim * n) } sub

main : IO ()
main =
  do (Right lines) <- fGetLines []
        | Left err => die $ show err
     (Just cmds) <- pure $ traverse toCmd lines
        | Nothing => die "Not all commands were valid."
     let resSub = foldr (flip step) (MkSub 0 0 0) cmds
     putStrLn $ "Final (d, hp): (" ++ show resSub.depth ++ ", " ++ show resSub.hPos ++ ")"
     putStrLn $ "Answer = " ++ show (resSub.depth * resSub.hPos)
  where
    fGetLines : HasIO io => List String -> io (Either FileError (List String))
    fGetLines acc = do (Right l) <- fGetLine stdin
                          | Left err => pure $ Left err
                       case l of
                            "" => pure $ Right acc
                            _  => fGetLines (l :: acc)

