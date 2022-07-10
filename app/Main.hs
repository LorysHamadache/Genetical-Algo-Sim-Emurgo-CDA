module Main where

-- Imports
import Graphics.Gloss
import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Interface.IO.Simulate

import Model
import Tests 

-- Window Definition

background = white
nbSimPerSec = 10
(window_w, window_h) = (1200,1200)




-- Main 

main :: IO ()
main = do
  (screen_w,screen_h) <- getScreenSize
  let (center_w, center_h) = ((screen_w - window_w) `div` 2, (screen_h - window_h) `div` 2 )

  let window = InWindow "Lorys - Haskell Project " (window_w, window_h) (center_w, center_h) 
  simulate window background nbSimPerSec model1 render_model update_model
  