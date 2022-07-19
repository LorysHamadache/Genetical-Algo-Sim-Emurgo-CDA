module Main where

-- Imports

import Types
import Render
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Interface.IO.Simulate
import System.Random


import Model

-- LAUNCH WITH 
--cabal haddock --haddock-executables  


background = color_dark
nbSimPerSec = 50
(window_w, window_h) = (1200,1200)

-- | The Main function is the opening function of our program
-- It consists of defining the initial state of our model and starting the simulation function
main :: IO ()
main = do
  let env_size = 1000
  let c1 = basic_character {name = "Lorys"}
  let c2 = basic_character {name = "Nicole", team = T2}
  let f1 = basic_food 
  let character_list = [c1,c2]
  let env1 = Environment env_size [f1]
  let model1 = Model env1 character_list

  
  (screen_w,screen_h) <- getScreenSize -- ^ Get User Screen Size in order to center the window on start (Useful especially for big screen 5120x1440)
  let (center_w, center_h) = ((screen_w - window_w) `div` 2, (screen_h - window_h) `div` 2 )
  let window = InWindow "Lorys - Haskell Project " (window_w, window_h) (center_w, center_h) -- ^ Define the window of the simulation

  simulateIO window background nbSimPerSec model1 render_model update_model