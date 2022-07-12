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


-- Window Definition


background = white
nbSimPerSec = 50
(window_w, window_h) = (1200,1200)


-- Other Definition


-- Main 

main :: IO ()
main = do
  -- Define Window
  putStrLn "Please input a large random number."
  random1 <- readLn :: IO Int
  random2 <- randomRIO (1,random1)
  let modelseed = mkStdGen (random1*random2)

  let env_size = 1000
  let c1 = Character "Lorys" Alive (-100,0) 0 10 1 100 Blue (mkStdGen random1)
  let c2 = Character "Nicole" Alive (-50,0) 0 10 1 100 Red (mkStdGen random2)
  let character_list = [c1,c2]
  let env1 = Environment env_size []
  let model1 = Model env1 character_list modelseed

  
  (screen_w,screen_h) <- getScreenSize
  let (center_w, center_h) = ((screen_w - window_w) `div` 2, (screen_h - window_h) `div` 2 )
  let window = InWindow "Lorys - Haskell Project " (window_w, window_h) (center_w, center_h)

  -- Launch Simulation
  simulate window background nbSimPerSec model1 render_model update_model
  