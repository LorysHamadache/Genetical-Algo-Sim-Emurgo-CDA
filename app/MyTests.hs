module MyTests where

import System.Random

main = do
  --why every single line prints "True" ?
  print $ fst (random (mkStdGen 41) :: (Bool, StdGen))
  print $ fst (random (mkStdGen 5) :: (Bool, StdGen))
  print $ fst (random (mkStdGen 454) :: (Bool, StdGen))
  print $ fst (random (mkStdGen 55645) :: (Bool, StdGen))
  print $ fst (random (mkStdGen 4545) :: (Bool, StdGen))
  print $ fst (random (mkStdGen 7545) :: (Bool, StdGen))
  print $ fst (random (mkStdGen 4545) :: (Bool, StdGen))
  print $ fst (random (mkStdGen 8) :: (Bool, StdGen))
  print $ fst (random (mkStdGen 9) :: (Bool, StdGen))
  print $ fst (random (mkStdGen 10) :: (Bool, StdGen))